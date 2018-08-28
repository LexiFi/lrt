open Dynt_core

(** Operations on ttypes. *)

(** {2 Safe inspection of ttypes} *)

type 'a record_builder

module RecordField : sig
  type ('s, 't) t
  (** Runtime representation of fields of type ['t] in records of type ['s].
      Also used to represent tuple components. *)

  val ttype: (_, 't) t -> 't ttype

  val name: _ t -> string
  (** Empty string for tuples. *)

  val props: _ t -> (string * string) list
  (** Empty list for tuples. *)

  val get: ('s, 't) t -> 's -> 't
  (** Extract the value corresponding to the given field of a record value. *)

  val set: ('s, 't) t -> 's record_builder -> 't -> unit
  (** To be used in the callback passed to [Record.make]. *)

  val path: ('s, 't) t -> ('s, 't) Path.field
end

type 's has_record_field = Field: ('s, 't) RecordField.t -> 's has_record_field

exception Missing_field_in_record_builder

type 's field_builder = { mk: 't. ('s, 't) RecordField.t -> 't }
  [@@ocaml.unboxed]

module Record: sig
  type 's t
  (** Runtime representation of records or tuples of type ['s]. *)

  val ttype: 's t -> 's ttype

  val fields: 's t -> 's has_record_field list

  val build: 's t -> 's field_builder -> 's
  (** Create a record value.  The polymorphic [mk] callback is guaranteed
      to be called in the same order as the [fields] list. *)

  val make: ?default:'s -> 's t -> ('s record_builder -> unit) -> 's
  (** Create a record value.  Contrary to [build], this function allows
      the client code to decide in which order fields are populated.

      If the default is not specified, the callback must call the
      [RecordField.set] function at least once on each field (passing
      the provided [record_builder]).  If the default is specified,
      it is used to populate fields which have not been set explicitly.
  *)

  val find_field: 's t -> string -> 's has_record_field option
  (** Extract the constructor corresponding to the given value of the
      sum type. *)
end

module Constructor : sig
  type ('s, 't) t
  (** Runtime representation of constructors of type ['t] in sum types of type ['s].
      The type of constructor is defined as follows:

      - A constructor without an argument has type "unit".
      - A constructor with multiple arguments has a tuple type.
      - A constructor with an inline record arguments has a pseudo-record type.
  *)

  val ttype: (_, 't) t -> 't ttype

  val index: _ t -> int

  val name: _ t -> string

  val props: _ t -> (string * string) list

  val inline: _ t -> bool
  (** Whether the constructor holds an inlined record. *)

  val project: ('s, 't) t -> 's -> 't option
  (** Check if a value of the sum type matches the given constructor;
      when this is the case, returns the constructor's argument. *)

  val project_exn: ('s, 't) t -> 's -> 't
  (** Raises Not_found if not the proper constructor.
      Faster than [project] when not in the error case. *)

  val inject:('s, 't) t -> 't -> 's
  (** Create a value of the sum type with the given constructor and
      its argument. *)

  val path: ('s, 't) t -> ('s, 't) Path.constructor
end

type 's has_constructor = Constructor: ('s, 't) Constructor.t -> 's has_constructor

module Sum: sig
  type 's t

  val ttype: 's t -> 's ttype

  val path: 's t -> string

  val constructors: 's t -> 's has_constructor array

  val get_constructor_index: 's t -> 's -> int
  (** Find the index (in [constructors]) of the constructor
      corresponding to the given value of the sum type. *)

  val constructor: 's t -> 's -> 's has_constructor
  (** Extract the constructor corresponding to the given value of the
      sum type. *)

  val lookup_constructor: 's t -> string -> int
  (** Find the index of the constructor with the given name.  Returns
      -1 if not found.  The first call to this function is slow (it
      builds an optimized lookup table).  *)
end

module Method: sig
  type ('s, 't) t
  (** Runtime representation of methods of type ['t] in objects of
      type ['s]. *)

  val ttype: (_, 't) t -> 't ttype

  val name: _ t -> string

  val call: ('s, 't) t -> 's -> 't
end

type 's has_method = Method: ('s, 't) Method.t -> 's has_method


module Object: sig
  type 's t
  (** Runtime representation of objects of type ['t]. *)

  val ttype: 's t -> 's ttype

  val methods: 's t -> 's has_method list
  (** Sorted by method name. *)

  (* TODO: add a lookup function (by method name). *)
end

type _ is_function = Function: (string * 'b ttype * 'c ttype) -> ('b -> 'c) is_function
type _ is_list = List: 't ttype -> ('t list) is_list
type _ is_array = Array: 't ttype -> ('t array) is_array
type _ is_option = Option: 't ttype -> ('t option) is_option
type _ is_tuple2 = Tuple2: ('a ttype * 'b ttype) -> ('a * 'b) is_tuple2
type _ is_tuple3 = Tuple3: ('a ttype * 'b ttype * 'c ttype) -> ('a * 'b * 'c) is_tuple3
(* type _ is_wlazy = Wlazy: 'b ttype * ('a, 'b Wlazy.t) TypEq.t -> 'a is_wlazy *)

val is_list: 'a ttype -> 'a is_list option
val is_array: 'a ttype -> 'a is_array option
val is_option: 'a ttype -> 'a is_option option
val is_function: 'a ttype -> 'a is_function option
val is_tuple2: 'a ttype -> 'a is_tuple2 option
val is_tuple3: 'a ttype -> 'a is_tuple3 option
(* val is_wlazy: 'a ttype -> 'a is_wlazy option *)

val is_record: 'a ttype -> 'a Record.t option
val is_tuple: 'a ttype -> 'a Record.t option
val is_sum: 'a ttype -> 'a Sum.t option
val is_object: 'a ttype -> 'a Object.t option
val is_prop: 'a ttype -> ((string * string) list * 'a ttype) option
val is_abstract: 'a ttype -> (string * 'a ttype * stype list) option

val make_abstract: 'a ttype -> 'a ttype

type 'a xtype
  = Unit: unit xtype
  | Bool: bool xtype
  | Int: int xtype
  | Float: float xtype
  | String: string xtype
  (* | Date: date xtype *)
  | Char: char xtype
  | Int32: int32 xtype
  | Int64: int64 xtype
  | Nativeint: nativeint xtype
  | Option: 'b ttype * 'b xtype Lazy.t -> 'b option xtype
  | List: 'b ttype * 'b xtype Lazy.t -> 'b list xtype
  | Array: 'b ttype * 'b xtype Lazy.t  -> 'b array xtype
  | Function: (string * ('b ttype * 'b xtype Lazy.t) * ('c ttype * 'c xtype Lazy.t)) -> ('b -> 'c) xtype
  | Sum: 'a Sum.t -> 'a xtype
  | Tuple: 'a Record.t -> 'a xtype
  | Record: 'a Record.t -> 'a xtype
  | Lazy: ('b ttype * 'b xtype Lazy.t) -> 'b Lazy.t xtype
  | Prop: ((string * string) list * 'a ttype * 'a xtype Lazy.t) -> 'a xtype
  | Object: 'a Object.t -> 'a xtype
  | Abstract: (string * 'a ttype * stype list) -> 'a xtype

val xtype_of_ttype: 'a ttype -> 'a xtype
val ttype_of_xtype: 'a xtype -> 'a ttype

val xtype_of_constructor: (_, 'a) Constructor.t -> 'a xtype
val xtype_of_field: (_, 'a) RecordField.t -> 'a xtype
val xtype_of_method: (_, 'a) Method.t -> 'a xtype

val get_first_props_xtype: 'a xtype -> (string * string) list

val remove_first_props_xtype: 'a xtype -> 'a xtype

type sttype = Ttype: 'a ttype -> sttype

val sttype_of_stype: stype -> sttype

val all_paths: root:'root ttype -> target:'target ttype -> ('root, 'target, Path.kind) Path.t list
(** Returns all the paths leading to a value of type ['target] inside
    a value of type ['root]. Does not traverse list, array, lazy, objects.
    Will loop on recursive types.
 *)

(*
 * Type Matchers
 *
 *)

module type TYPE_0 = sig
  type t
  val t: t ttype
end

module type TYPE_1 = sig
  type 'a t
  val t: 'a ttype -> 'a t ttype
end

module type TYPE_2 = sig
  type ('a, 'b) t
  val t: 'a ttype -> 'b ttype -> ('a, 'b) t ttype
end

module type MATCHER_0 = sig
  include TYPE_0
  type _ is_t = Is: ('a, t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a ttype -> 'a is_t option
  val is_abstract: string option
end

module type MATCHER_1 = sig
  include TYPE_1
  type _ is_t = Is: 'b ttype * ('a, 'b t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a ttype -> 'a is_t option
  val is_abstract: string option
end

module type MATCHER_2 = sig
  include TYPE_2
  type _ is_t = Is: 'b ttype * 'c ttype * ('a, ('b, 'c) t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a ttype -> 'a is_t option
  val is_abstract: string option
end

module Matcher_0 (T : TYPE_0) : MATCHER_0 with type t = T.t
module Matcher_1 (T : TYPE_1) : MATCHER_1 with type 'a t = 'a T.t
module Matcher_2 (T : TYPE_2) : MATCHER_2 with type ('a, 'b) t = ('a, 'b) T.t

