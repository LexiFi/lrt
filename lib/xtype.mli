(** Safe inspection of runtime types. *)

type record_repr = Regular | Float | Unboxed
type constr_repr = Tag of int | Unboxed

type 'a t = private
  { t: 'a Ttype.t
  ; xt: 'a xtype Lazy.t
  }

and 'a xtype = private
  | Unit: unit xtype
  | Bool: bool xtype
  | Int: int xtype
  | Float: float xtype
  | String: string xtype
  | Char: char xtype
  | Int32: int32 xtype
  | Int64: int64 xtype
  | Nativeint: nativeint xtype
  | List: 'b t -> 'b list xtype
  | Option: 'b t -> 'b option xtype
  | Array: 'b t -> 'b array xtype
  | Lazy: 'b t -> 'b Lazy.t xtype
  | Tuple: 'a tuple -> 'a xtype
  | Record: 'a record -> 'a xtype
  | Sum: 'a sum -> 'a xtype
  | Function: ('b,'c) arrow -> ('b -> 'c) xtype
  | Object: 'a object_ -> 'a xtype
  | Prop: (Stype.properties * 'a t) -> 'a xtype
  | Abstract: (string * Stype.t list) -> 'a xtype

and ('s,'t) element = private
  { typ: 't t
  ; nth: int
  }

and 's field = private
  | Field: ('s, 't) element -> 's field

and 's tuple = private
  { t_flds : 's field list }

and label = string * Stype.properties

and 's record_field = label * 's field

and 's record = private
  { r_flds: 's record_field list
  ; r_repr: record_repr
  }

and 's constant_constructor = private
  { cc_label: label
  ; cc_nr: int
  }

and ('s, 't) regular_constructor  = private
  { rc_label: label
  ; rc_flds: 't field list
  ; rc_repr: constr_repr
  }

and ('s, 't) inlined_constructor  = private
  { ic_label: label
  ; ic_flds: 't record_field list
  ; ic_repr: constr_repr
  }

and 's constructor = private
  | Constant : 's constant_constructor -> 's constructor
  | Regular : ('s, 't) regular_constructor -> 's constructor
  | Inlined : ('s, 't) inlined_constructor -> 's constructor

and 's sum = private
  { cstrs : 's constructor list }

and ('s, 't) arrow = private
  { arg_label : string option
  ; arg_t: 's t
  ; res_t: 't t
  }

and 's method_ = private
  | Method: string * ('s, 't) element -> 's method_

and 's object_ = private
  { methods : 's method_ list }

(** There was a [ttype_of_xtype] function before. I think this encourages bad
    style, i.e. forcing an xtype too early and then going back to ttype.
    Getting rid of this function allowed to ditch some ugly parts of the xtypes
    implementation. *)

val xtype_of_ttype: 'a Ttype.t -> 'a xtype
val of_ttype: 'a Ttype.t -> 'a t

module Fields : sig
  val tuple : 'a tuple -> ('a, 'b) element -> 'a -> 'b
  val record : 'a record -> ('a, 'b) element -> 'a -> 'b
  val regular_constructor :
    ('a, 'b) regular_constructor -> ('b, 'c) element -> 'a -> 'c option
  val inlined_constructor :
    ('a, 'b) inlined_constructor -> ('b, 'c) element -> 'a -> 'c option

  val map_tuple : 'a tuple -> (Ttype.dynamic -> 'b) -> 'a -> 'b list
  val map_record :
    'a record -> (name:string -> Ttype.dynamic -> 'b) -> 'a -> 'b list
  val map_regular : ('a, 'b) regular_constructor ->
    (Ttype.dynamic -> 'c) -> 'a -> 'c list
  val map_inlined : ('a, 'b) inlined_constructor ->
    (name:string -> Ttype.dynamic -> 'c) -> 'a -> 'c list
end
(** Read values from tuples, records and constructors. *)

(** {2 Find elements} *)

module Lookup : sig
  val record_field: 'a record -> string -> 'a record_field option
  val constructor: 'a sum -> string -> 'a constructor option
  val constructor_field:
    ('a, 'b) inlined_constructor -> string -> 'b record_field option
  val method_: 'a object_ -> string -> 'a method_ option
end
(** Find by name. *)

val constructor_by_value: 'a sum -> 'a -> 'a constructor
(** Find by value. *)

(** {2 Object call method} *)

val call_method: 'a object_ -> ('a, 'b) element -> 'a -> 'b

(** {2 Building values from xtypes} *)

module Builder : sig
  (** The builder function [mk] is called for each field in the order of the
      fields array.
  *)

  type 's t = { mk: 't. ('s, 't) element -> 't } [@@unboxed]
  type 's t' = { mk: 't. label -> ('s, 't) element -> 't } [@@unboxed]

  val tuple : 'a tuple -> 'a t -> 'a
  val record : 'a record -> 'a t' -> 'a
  val constant_constructor : 'a constant_constructor -> 'a
  val regular_constructor : ('a, 'b) regular_constructor -> 'b t -> 'a
  val inlined_constructor : ('a, 'b) inlined_constructor -> 'b t' -> 'a

  type generic = { mk: 's 't. ('s, 't) element -> 't } [@@unboxed]
  val constructor : 'a constructor -> generic -> 'a
end
(** Building values from xtypes. *)

module Make : sig
  type 'a t
  exception Missing_field of string

  val set: 'a t -> ('a, 'b) element -> 'b -> unit

  val tuple: 'a tuple -> ('a t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)

  val record: 'a record -> ('a t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)

  val regular_constructor:
    ('a, 'b) regular_constructor -> ('b t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)

  val inlined_constructor:
    ('a, 'b) inlined_constructor -> ('b t -> unit) -> 'a
  (** Throws [Missing_field] if not all fields where set via [set]. *)
end
(** Similar to [Builder] but with active interface. *)

(** {2 Paths} *)

module Step : sig
  val tuple: 'a tuple -> ('a, 'b) element -> ('a, 'b) Path.step
  val record: 'a record -> ('a, 'b) element -> ('a, 'b) Path.step
  val regular_constructor:
    ('a, 'b) regular_constructor -> ('b, 'c) element -> ('a, 'c) Path.step
  val inlined_constructor:
    ('a, 'b) inlined_constructor -> ('b, 'c) element -> ('a, 'c) Path.step
end
(** Build steps from elements. *)

val all_paths: 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) Path.t list
(** Returns all the paths leading to a value of type ['a] inside
    a value of type ['b]. Does not traverse list, array, lazy, objects.
    Will loop on recursive types. *)

val project_path : 'a Ttype.t -> ('a,'b) Path.t -> 'b Ttype.t
(** Extraction of sub-type pointed to by a path. *)

(** {2 Type Matchers}
    Compare nontrivial types. *)

module type T0 = sig
  type t
  val t: t Ttype.t
end

module type T1 = sig
  type 'a t
  val t: 'a Ttype.t -> 'a t Ttype.t
end

module type T2 = sig
  type ('a, 'b) t
  val t: 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t Ttype.t
end

module type MATCH0 = sig
  include T0
  type _ is_t = Is: ('a, t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a Ttype.t -> 'a is_t option
  val is_abstract: string option
  (** If [t] is an abstract type, [is_abstract t] provides its identifier.*)
end

module type MATCH1 = sig
  include T1
  type _ is_t = Is: 'b Ttype.t * ('a, 'b t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a Ttype.t -> 'a is_t option
  val is_abstract: string option
  (** If [t] is an abstract type, [is_abstract t] provides its identifier.*)
end

module type MATCH2 = sig
  include T2
  type _ is_t =
      Is: 'b Ttype.t * 'c Ttype.t * ('a, ('b, 'c) t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a Ttype.t -> 'a is_t option
  val is_abstract: string option
  (** If [t] is an abstract type, [is_abstract t] provides its identifier.*)
end

module Match0 (T : T0) : MATCH0 with type t = T.t
module Match1 (T : T1) : MATCH1 with type 'a t = 'a T.t
module Match2 (T : T2) : MATCH2 with type ('a, 'b) t = ('a, 'b) T.t

