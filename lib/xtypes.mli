(** Safe inspection of runtime types. *)

open Dynt_core
open Dynt_core.Ttype
open Dynt_core.Stype

type record_repr = Regular | Float | Unboxed
type constr_repr = Tag of int | Unboxed

type 'a t = 'a ttype * 'a xtype Lazy.t
(** The construction of xtypes is expensive and should not happen recursively.*)

and 'a xtype
  = Unit: unit xtype
  | Bool: bool xtype
  | Int: int xtype
  | Float: float xtype
  | String: string xtype
  | Char: char xtype
  | Int32: int32 xtype
  | Int64: int64 xtype
  | Nativeint: nativeint xtype
  | Option: 'b t -> 'b option xtype
  | List: 'b t -> 'b list xtype
  | Array: 'b t -> 'b array xtype
  | Lazy: 'b t -> 'b Lazy.t xtype
  | Tuple: 'a tuple -> 'a xtype
  | Record: 'a record -> 'a xtype
  | Sum: 'a sum -> 'a xtype
  | Function: ('b,'c) arrow -> ('b -> 'c) xtype
  | Object: 'a object_ -> 'a xtype
  | Prop: (stype_properties * 'a t) -> 'a xtype
  | Abstract: (string * stype list) -> 'a xtype

and label = string * stype_properties
and ('s,'t) element = { typ: 't t; nth: int } (** Better name? *)

and 's field = Field: ('s, 't) element -> 's field
and 's tuple = 's field array
and 's record_field = label * 's field
and 's record = 's record_field array * record_repr

and 's constant_constructor = label * int
and 's regular_constructor  = label * 's field array * constr_repr
and 's inlined_constructor  = label * 's record_field array * constr_repr

and 's constructor =
  | Constant of 's constant_constructor
  | Regular of 's regular_constructor
  | Inlined of 's inlined_constructor

and 's sum = 's constructor array

and ('s, 't) arrow =
  { arg_label : string option
  ; arg_t: 's t
  ; res_t: 't t
  }

and 's method_ = Method: string * ('s, 't) element -> 's method_
and 's object_ = 's method_ array

(** {2 Basics} *)

(** There was a [ttype_of_xtype] function before. I think this encourages bad
    style, i.e. forcing an xtype too early and then going back to ttype.
    Getting rid of this function allowed to ditch some ugly parts of the xtypes
    implementation. *)

val xtype_of_ttype: 'a ttype -> 'a xtype
val t_of_ttype: 'a ttype -> 'a t

val get_first_props_xtype: 'a xtype -> stype_properties
val remove_first_props_xtype: 'a xtype -> 'a xtype

(** {2 Find named elements} *)

module Lookup : sig
  val record_field: 'a record -> string -> 'a record_field option
  val constructor: 'a sum -> string -> 'a constructor option
  val constructor_field:
    'a inlined_constructor -> string -> 'a record_field option
  val method_: 'a object_ -> string -> 'a method_ option

  val constructor_by_value: 'a sum -> 'a -> 'a constructor
end

(** {2 Object call method} *)

val call_method: 'a object_ -> ('a, 'b) element -> 'a -> 'b

(** {2 Building values from xtypes} *)

module Builder : sig
  (** Building values from xtypes.

      The builder function [mk] is called for each field in the order of the
      fields array.
  *)

  type 'a t = { mk: 't. ('a, 't) element -> 't } [@@unboxed]

  val tuple : 'a tuple -> 'a t -> 'a
  val record : 'a record -> 'a t -> 'a
  val constant_constructor : 'a constant_constructor -> 'a
  val regular_constructor : 'a regular_constructor -> 'a t -> 'a
  val inlined_constructor : 'a inlined_constructor -> 'a t -> 'a
  val constructor : 'a constructor -> 'a t -> 'a
end

module Make : sig
  (** Similar to [Builder] but with active interface instead of passive *)

  type 'a t
  exception Missing_field of string

  val set: 'a t -> ('a, 'b) element -> 'b -> unit

  val tuple: 'a tuple -> ('a t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)

  val record: 'a record -> ('a t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)

  val constructor: 'a constructor -> ('a t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)
end

(** {2 Paths} *)

module Step : sig
  val tuple: 'a tuple -> ('a, 'b) element -> ('a,'b) Path.step
  val record: 'a record -> ('a, 'b) element -> ('a,'b) Path.step
  val regular_constructor:
    'a regular_constructor -> ('a,'b) element -> ('a,'b) Path.step
  val inlined_constructor:
    'a inlined_constructor -> ('a,'b) element -> ('a,'b) Path.step

  val constructor : 'a constructor -> ('a,'b) element -> ('a,'b) Path.step

  (** TODO: It is possible to use an element of different constructor. This
      leads to unspecified behaviour including SEGV *)
end

val all_paths: 'a ttype -> 'b ttype -> ('a, 'b) Path.t list
(** Returns all the paths leading to a value of type ['a] inside
    a value of type ['b]. Does not traverse list, array, lazy, objects.
    Will loop on recursive types. *)

val project_path : 'a ttype -> ('a,'b) Path.t -> 'b ttype
(** Extraction of sub-type pointed to by a path. *)

(** {2 Type Matchers}
    Compare nontrivial ttypes with each other. *)

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

