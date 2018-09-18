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

and ('s,'t) field =
  { t: 't t
  ; nth: int
  }

and 's has_field = Field: ('s, 't) field -> 's has_field

and 's tuple = 's has_field array

and ('s, 't) named_field =
  { field: ('s, 't) field
  ; field_name: string (* Move into FieldFinder *)
  ; field_props: stype_properties
  }

and 's has_named_field = NamedField: ('s, 't) named_field -> 's has_named_field

and 's named_tuple =
  { fields: 's has_named_field array
  ; find_field: string -> 's has_named_field option
  }

and 's record = 's named_tuple * record_repr

and ('s, _) constr_kind =
  | Constant : int -> ('s, [> `Constant]) constr_kind
  | Regular : 's tuple * constr_repr -> ('s, [> `Regular]) constr_kind
  | Inlined : 's named_tuple * constr_repr -> ('s, [> `Inlined]) constr_kind

and ('s, 'kind) constructor =
  { constr_name: string
  ; constr_props: stype_properties
  ; kind: ('s, 'kind) constr_kind
  }

and 's has_constructor = ('s, [`Constant|`Regular|`Inlined]) constructor

and 's sum =
  { constructors: 's has_constructor array
  ; find_constructor: string -> 's has_constructor option
  ; constructor: 's -> 's has_constructor
  }

and ('s, 't) arrow =
  { label : string option
  ; from_t: 's t
  ; to_t: 't t
  }

and ('s, 't) method_ =
  { method_name: string
  ; method_type: 't t
  ; call: 's -> 't
  }

and 's has_method = Method: ('s, 't) method_ -> 's has_method

and 's object_ =
  { methods : 's has_method array
  ; find_method : string -> 's has_method option
  }

type 's constant_constructor = ('s, [`Constant]) constructor
type 's regular_constructor = ('s, [`Regular]) constructor
type 's inlined_constructor = ('s, [`Inlined]) constructor

(** {2 Basics} *)

(** There was a [ttype_of_xtype] function before. I think this encourages bad
    style, i.e. forcing an xtype too early and then going back to ttype.
    Getting rid of this function allowed to ditch some ugly parts of the xtypes
    implementation. *)

val xtype_of_ttype: 'a ttype -> 'a xtype
val t_of_ttype: 'a ttype -> 'a t

val get_first_props_xtype: 'a xtype -> stype_properties
val remove_first_props_xtype: 'a xtype -> 'a xtype

(** {2 Building values from xtypes} *)

module Builder : sig
  (** Building values from xtypes.

      The builder function [mk] is called for each field in the order of the
      fields array.
  *)

  type 'a t = { mk: 't. ('a, 't) field -> 't } [@@unboxed]
  type 'a named = { mk: 't. ('a, 't) named_field -> 't } [@@unboxed]

  val tuple : 'a tuple -> 'a t -> 'a
  val record : 'a record -> 'a named -> 'a
  val constant_constructor : 'a constant_constructor -> 'a
  val regular_constructor : 'a regular_constructor -> 'a t -> 'a
  val inlined_constructor : 'a inlined_constructor -> 'a named -> 'a
end

module Make : sig
  (** Similar to [Builder] but with active interface instead of passive *)

  type 'a t
  exception Missing_field of string

  val set: 'a t -> ('a, 'b) field -> 'b -> unit

  val tuple: 'a tuple -> ('a t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)

  val record: 'a record -> ('a t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)

  val constructor: ('a, [`Constant | `Inlined | `Regular ]) constructor
    -> ('a t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)
end

(** {2 Paths} *)

module Step : sig
  val tuple: 'a tuple -> ('a, 'b) field -> ('a,'b) Path.step
  val record: 'a record -> ('a, 'b) named_field -> ('a,'b) Path.step
  val regular_constructor:
    'a regular_constructor -> ('a,'b) field -> ('a,'b) Path.step
  val inlined_constructor:
    'a inlined_constructor -> ('a,'b) named_field -> ('a,'b) Path.step
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

