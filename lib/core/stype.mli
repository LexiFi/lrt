(** Untyped representation of types. *)

type t = node gtype

and 'node gtype =
  | DT_node of 'node
  | DT_int
  | DT_float
  | DT_string
  | DT_tuple of 'node gtype list
  | DT_list of 'node gtype
  | DT_array of 'node gtype
  | DT_option of 'node gtype
  | DT_abstract of string * 'node gtype list
  | DT_arrow of string * 'node gtype * 'node gtype
  | DT_object of (string * 'node gtype) list
  | DT_prop of properties * 'node gtype
  | DT_var of int


and node = private {
  mutable rec_descr: node_descr; (** Structure. *)
  rec_uid: int; (** Corresponds to physical equality of nodes. *)
  rec_name: string; (** Fully qualified name. *)
  rec_args: t list; (** Arguments. *)
  mutable rec_has_var: bool option; (** Internal use only. *)
  mutable rec_hash: int; (** Internal use only. *)
  mutable rec_memoized: memoized_type_prop array;
}

and properties = (string * string) list

and memoized_type_prop = ..

and node_descr =
  | DT_variant of variant_descr
  | DT_record of record_descr

and record_descr = {
    record_fields: (string * properties * t) list;
    record_repr: record_repr;
  }

and record_repr = Record_regular | Record_float | Record_unboxed
                 | Record_inline of int

and variant_descr = {
  variant_constrs: (string * properties * t variant_args) list;
  variant_repr: variant_repr;
}

and 'stype variant_args =
  | C_tuple of 'stype list
  | C_inline of 'stype

and variant_repr = Variant_regular | Variant_unboxed

val print_ref: (Format.formatter -> t -> unit) ref
(** Pretty-printer hook. This can be changed dynamically. *)

val print: Format.formatter -> t -> unit
(** Pretty-printer for [Stype.t]. Calls {!print_ref}. *)

val print_hide_enumerations: Format.formatter -> t -> unit

val strict_equality: t -> t -> bool
val equality: t -> t -> bool
val equality_modulo_props: t -> t -> bool

val remove_outer_props: t -> t

val uninline : 'a variant_args -> 'a list
val is_cst_args : 'a variant_args -> bool

module Internal: sig
  (** Internally used helper functions *)

  val create_variant_type: string -> t list
    -> (t -> (string * properties * t variant_args) list * variant_repr) -> t

  val create_record_type: string -> t list
    -> (t -> (string * properties * t) list * record_repr) -> t

  val create_node: string -> t list -> node

  val set_node_variant: node
    -> ((string * properties * t variant_args) list * variant_repr) -> unit

  val set_node_record: node
    -> (((string * properties * t) list) * record_repr) -> unit

  val hash0: t -> int
  (** This hash function ignores paths and properties. It remembers only the
   * ordered list of constructors names (with arity) and field labels. It
   * memoized the hash value of nodes. *)

  val hash0_node: node -> int


  val hash: ignore_props:bool -> ignore_path:bool -> t -> int

  val equal: ignore_props:bool -> ignore_path:bool -> t -> t -> bool
  (** The function returned (after passing the two named arguments) is memoized.
   * *)


  val has_var: t -> bool
  val substitute: t array -> t -> t
  (** Substitute all the DT_var nodes in the second arguments with elements of
      the array. *)

  val normalize: ignore_props:bool -> ignore_path:bool -> t -> t
  (** The function returned (after passing the two named arguments) is memoized.
   * *)

  val remove_props: t -> t
  (** This function is memoized. *)

  val set_memoized: node -> memoized_type_prop array -> unit
end

module Textual: sig
  (** This module defines a tree representation isomorphic to the internal
      graphs. This form is useful for printing and storing stypes. *)
  type stype = t

  type t = int gtype

  type node =
    | Variant of
        string * t list * (string * properties * t variant_args) list * variant_repr
    | Record of
        string * t list * (string * properties * t) list * record_repr

  type textual = {
    nodes: node array;
    t: t;
  }

  val export: stype -> textual
  val export_with_digests: stype -> textual * string array

  val import: textual -> stype

  val import_table: textual -> string array -> stype array
end
