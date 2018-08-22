(** Runtime representation of types and type equalities. *)

(** {2 Type equalities} *)

module TypEq : sig
  type (_, _) t = Eq: ('a, 'a) t
  (** A value of type [('a, 'b) t] is a witness that the two types ['a] and
   * ['b] are equal. *)

  val refl: ('a, 'a) t
  val trans: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val sym: ('a, 'b) t -> ('b, 'a) t

  val app: ('a, 'b) t -> 'a -> 'b

  module Lift(T : sig type 'a c end) : sig
    val eq: ('a, 'b) t -> ('a T.c, 'b T.c) t
  end
end

(** {2 Untyped type description} *)

type stype_properties = (string * string) list

type record_repr = Record_regular | Record_float | Record_inline of int

type 'node gtype =
  | DT_node of 'node
  | DT_int
  | DT_float
  | DT_string
  (* | DT_date *)
  | DT_tuple of 'node gtype list
  | DT_list of 'node gtype
  | DT_array of 'node gtype
  | DT_option of 'node gtype
  | DT_abstract of string * 'node gtype list
  | DT_arrow of string * 'node gtype * 'node gtype
  | DT_object of (string * 'node gtype) list
  | DT_prop of stype_properties * 'node gtype
  | DT_var of int

type memoized_type_prop = ..

type stype = node gtype
and node = private {
  mutable rec_descr: node_descr; (** Structure. *)
  rec_uid: int; (** Corresponds to physical equality of nodes. *)
  rec_name: string; (** Fully qualified name. *)
  rec_args: stype list; (** Arguments. *)
  mutable rec_has_var: bool option; (** Internal use only. *)
  mutable rec_hash: int; (** Internal use only. *)
  mutable rec_memoized: memoized_type_prop array;
}

(** Instantiated datatype declaration. *)

and node_descr =
  | DT_variant of variant_descr
  | DT_record of record_descr
and variant_descr = {
  variant_constrs: (string * stype_properties * stype variant_args) list;
}
and 'stype variant_args =
  | C_tuple of 'stype list
  | C_inline of 'stype
  (* [@@mlfi.dyn {of_variant_inject_on_type_error="C_tuple INJECTION"}] *)

and record_descr = {
  record_fields: (string * stype_properties * stype) list;
  record_repr: record_repr;
}

module Textual: sig
  (** This module defines a tree representation isomorphic to the internal
      graphs. This form is useful for printing and storing stypes. *)

  type t = int gtype

  type node =
    | Variant of
        string * t list * (string * stype_properties * t variant_args) list
    | Record of
        string * t list * (string * stype_properties * t) list * record_repr

  type textual = {
    nodes: node array;
    t: t;
  }

  val export: stype -> textual
  val export_with_digests: stype -> textual * string array

  val import: textual -> stype

  val import_table: textual -> string array -> stype array
end

val print_stype_ref: (Format.formatter -> stype -> unit) ref
(** Pretty-printer hook. This can be changed dynamically. *)

val print_stype: Format.formatter -> stype -> unit
(** Pretty-printer for stype. Calls {!print_stype_ref}. *)

val print_stype_hide_enumerations: Format.formatter -> stype -> unit

val strict_types_equality: stype -> stype -> bool
val types_equality: stype -> stype -> bool
val types_equality_modulo_props: stype -> stype -> bool

val remove_first_props: stype -> stype
val uninline : 'a variant_args -> 'a list
val is_cst_args : 'a variant_args -> bool

(** {2 Typed type description} *)

type 'a ttype
external stype_of_ttype: _ ttype -> stype = "%identity"

val ttypes_equality: 'a ttype -> 'b ttype -> ('a, 'b) TypEq.t option
val ttypes_equality_modulo_props: 'a ttype -> 'b ttype
  -> ('a, 'b) TypEq.t option

val split_arrow_ttype: ('a -> 'b) ttype -> 'a ttype * 'b ttype
val build_arrow_ttype: 'a ttype -> 'b ttype -> ('a -> 'b) ttype

val ttype_fst: ('a * 'b) ttype -> 'a ttype
val ttype_snd: ('a * 'b) ttype -> 'b ttype

val remove_first_props_ttype: 'a ttype -> 'a ttype
val add_props: (string * string) list -> 'a ttype -> 'a ttype

val abstract_ttype: 'a ttype -> 'a ttype
val abstract_stype: stype -> stype

(** {2 Internal definitions} *)

module Internal: sig
  val create_variant_type: string -> stype list
    -> (stype -> (string * stype_properties * stype variant_args) list)
    -> stype

  val create_record_type: string -> stype list
    -> (stype -> (string * stype_properties * stype) list * record_repr)
    -> stype

  val create_node: string -> stype list -> node

  val set_node_variant: node
    -> ((string * stype_properties * stype variant_args) list)
    -> unit

  val set_node_record: node
    -> (((string * stype_properties * stype) list) * record_repr)
    -> unit

  val hash0: stype -> int
  (** This hash function ignores paths and properties. It remembers only the
   * ordered list of constructors names (with arity) and field labels. It
   * memoized the hash value of nodes. *)

  val hash0_node: node -> int


  val hash: ignore_props:bool -> ignore_path:bool -> stype -> int

  val equal: ignore_props:bool -> ignore_path:bool -> stype -> stype -> bool
  (** The function returned (after passing the two named arguments) is memoized.
   * *)


  val has_var: stype -> bool
  val substitute: stype array -> stype -> stype
  (** Substitute all the DT_var nodes in the second arguments with elements of
      the array. *)

  val normalize: ignore_props:bool -> ignore_path:bool -> stype -> stype
  (** The function returned (after passing the two named arguments) is memoized.
   * *)

  val remove_props: stype -> stype
  (** This function is memoized. *)

  val set_memoized: node -> memoized_type_prop array -> unit
end

(**/**)

external internal_ttype_of: 'a -> 'a ttype = "%typeof"
external __use_ttype: 'a ttype -> 'b -> 'b = "%use_ttype"
