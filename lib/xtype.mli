(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

(** Visitable representation of types. *)

type record_repr = Regular | Float | Unboxed
type constr_repr = Tag of int | Unboxed

type 'a t = private {t: 'a Ttype.t; xt: 'a xtype Lazy.t}

and 'a xtype = private
  | Unit : unit xtype
  | Bool : bool xtype
  | Int : int xtype
  | Float : float xtype
  | String : string xtype
  | Char : char xtype
  | Int32 : int32 xtype
  | Int64 : int64 xtype
  | Nativeint : nativeint xtype
  | List : 'b t -> 'b list xtype
  | Option : 'b t -> 'b option xtype
  | Array : 'b t -> 'b array xtype
  | Lazy : 'b t -> 'b Lazy.t xtype
  | Tuple : 'a tuple -> 'a xtype
  | Record : 'a record -> 'a xtype
  | Sum : 'a sum -> 'a xtype
  | Function : ('b, 'c) arrow -> ('b -> 'c) xtype
  | Object : 'a object_ -> 'a xtype
  | Prop : (Stype.properties * 'a t) -> 'a xtype
  | Abstract : (string * Stype.t list) -> 'a xtype

and ('s, 't) element = private {typ: 't t; nth: int}

and 's field = private Field : ('s, 't) element -> 's field

and 's tuple = private {t_flds: 's field list; t_len: int}

and label = string * Stype.properties

and 's record_field = label * 's field

and 's record = private
  { r_flds: 's record_field list
  ; r_len: int
  ; r_repr: record_repr
  ; r_lookup: string -> 's record_field option }

and 's constant_constructor = private {cc_label: label; cc_nr: int}

and ('s, 't) regular_constructor = private
  {rc_label: label; rc_flds: 't field list; rc_len: int; rc_repr: constr_repr}

and ('s, 't) inlined_constructor = private
  { ic_label: label
  ; ic_flds: 't record_field list
  ; ic_len: int
  ; ic_repr: constr_repr
  ; ic_lookup: string -> 't record_field option }

and 's constructor = private
  | Constant : 's constant_constructor -> 's constructor
  | Regular : ('s, 't) regular_constructor -> 's constructor
  | Inlined : ('s, 't) inlined_constructor -> 's constructor

and 's sum = private
  { s_cstrs: 's constructor list
  ; s_lookup: string -> 's constructor option
  ; s_cstr_by_value: 's -> 's constructor }

and ('s, 't) arrow = private
  {arg_label: string option; arg_t: 's t; res_t: 't t}

and 's method_ = private Method : string * ('s, 't) element -> 's method_

and 's object_ = private
  {o_methods: 's method_ list; o_lookup: string -> 's method_ option}

(** {3 Initialization and transformation} *)

val of_ttype : 'a Ttype.t -> 'a t
(** Produce a representation of a runtime type, that is safer to inspect than
    the {!Stype.t} one. *)

val xtype_of_ttype : 'a Ttype.t -> 'a xtype
(** See {!of_ttype}. This produces the {!xtype} without wrapping it in {!t}. *)

(* There is intentionally no [ttype_of_xtype] function. It would encourage
   bad style, i.e. forcing an {!xtype} too early and then going back to
   {!ttype}. *)

val remove_outer_props : 'a t -> 'a t
(** Strip the outer properties from the type. The result is guaranteed to be
    different from {!const:Prop}. *)

val consume_outer_props : 'a t -> Stype.properties * 'a t
(** See {!remove_outer_props}. This function additionally returns an
    accumulated list of the removed properties. The accumulation is
    associative, i.e. [ Prop ([a], Prop ([b, c], _)) ] yields the same list
    as [ Prop ([a, b], Prop ([c], _)) ]. *)

(** {3 Destruction and construction of values} *)

(** Read values from tuples, records and constructors. *)
module Read : sig
  val tuple : 'a tuple -> ('a, 'b) element -> 'a -> 'b
  val record : 'a record -> ('a, 'b) element -> 'a -> 'b

  val regular_constructor 
    : ('a, 'b) regular_constructor -> ('b, 'c) element -> 'a -> 'c option

  val inlined_constructor 
    : ('a, 'b) inlined_constructor -> ('b, 'c) element -> 'a -> 'c option

  val call_method : 'a object_ -> ('a, 'b) element -> 'a -> 'b

  (** The following functions allow to map the values stored in a tuple, record,
      or constructor to a list. This supports partial application, i.e.
      [map_tuple tup f] results in a closure that can processes many values while
      calling [f] only once on the first value.
  *)

  (** TODO: Instead of the Xtype.t, [f] could also consume a {!field}. *)
  type 'b mapf = {f: 'a. 'a t -> 'a -> 'b} [@@unboxed]

  val map_tuple : 'a tuple -> 'b mapf -> 'a -> 'b list
  (** Map the values in a tuple to list. *)

  (** TODO: Instead of the name and type, [f] may as well consume a
      {!record_field}. *)
  type 'b mapf' = {f: 'a. name:string -> 'a t -> 'a -> 'b} [@@unboxed]

  val map_record : 'a record -> 'b mapf' -> 'a -> 'b list
  (** Map the values in a record to a list. *)

  (** The constructor's name and its mapped arguments. *)
  type ('a, 'b) mapped_sum =
    | Regular of string * 'a list
    | Inlined of string * 'b list
    | Constant of string

  val map_sum : 'a sum -> 'b mapf -> 'c mapf' -> 'a -> ('b, 'c) mapped_sum
  (** Map the values in a constructor to a list. *)
end

(** Building tuples, records and constructors. *)
module Builder : sig
  (** The builder function [mk] is called for each field in the order of the
      fields list, i.e. in same order as they appear in the corresponding type
      definition.
  *)

  type 's t = {mk: 't. ('s, 't) element -> 't} [@@unboxed]
  type 's t' = {mk: 't. label -> ('s, 't) element -> 't} [@@unboxed]

  val tuple : 'a tuple -> 'a t -> 'a
  val record : 'a record -> 'a t' -> 'a
  val constant_constructor : 'a constant_constructor -> 'a
  val regular_constructor : ('a, 'b) regular_constructor -> 'b t -> 'a
  val inlined_constructor : ('a, 'b) inlined_constructor -> 'b t' -> 'a

  type generic = {mk: 's 't. ('s, 't) element -> 't} [@@unboxed]

  val constructor : 'a constructor -> generic -> 'a
end

(** Similar to {!Builder} but with active interface. *)
module Make : sig
  (** Instead of providing a function to the builder, that is then called for
      each field of the block, this module lets the user actively set each
      field.
  *)

  (** Raised if not all fields where set via {!set}. *)
  exception Missing_field of string

  (** An intermediate representation of a block. *)
  type 'a t

  val set : 'a t -> ('a, 'b) element -> 'b -> unit
  (** Set a single field of a block. *)

  val tuple : 'a tuple -> ('a t -> unit) -> 'a
  val record : 'a record -> ('a t -> unit) -> 'a

  val regular_constructor 
    : ('a, 'b) regular_constructor -> ('b t -> unit) -> 'a

  val inlined_constructor 
    : ('a, 'b) inlined_constructor -> ('b t -> unit) -> 'a
end

(** More restrictive interface to {!Builder} that allows to build closures. *)
module Assembler : sig
  (** The functions {!tuple}, {!record}, and {!sum} can be partially applied
      with the first two arguments.  This results in efficient closures that
      call the function in {!asm} only once per field definition instead of once
      per processed value.
  *)

  type 'a asm = {f: 'b. 'b t -> 'a -> 'b} [@@unboxed]

  val tuple : 'a tuple -> 'b asm -> 'b list -> 'a
  (** The assembler consumes the provided list in order of the tuple elements.*)

  val record : 'a record -> 'b asm -> (string * 'b) list -> 'a
  (** The assembler is efficient, when the provided alist is in the correct
      order.  If not, it falls back to {!List.assoc}. TODO: improve the fallback
      to use a table. *)

  type ('a, 'b) cstr =
    | Constant : 'a constant_constructor -> ('a, 'b) cstr
    | Regular : ('a, 'c) regular_constructor * 'b list -> ('a, 'b) cstr
    | Inlined :
        ('a, 'c) inlined_constructor * (string * 'b) list
        -> ('a, 'b) cstr

  val sum : 'a sum -> 'b asm -> ('a, 'b) cstr -> 'a
  (** The assembler uses the same mechanism as {!tuple} and {!sum}. *)
end

(** {3 Paths}

    Interop between runtime types and {!Path}.
*)

(** Build steps from elements. *)
module Step : sig
  val tuple : 'a tuple -> ('a, 'b) element -> ('a, 'b) Path.step
  val record : 'a record -> ('a, 'b) element -> ('a, 'b) Path.step

  val regular_constructor 
    : ('a, 'b) regular_constructor -> ('b, 'c) element -> ('a, 'c) Path.step

  val inlined_constructor 
    : ('a, 'b) inlined_constructor -> ('b, 'c) element -> ('a, 'c) Path.step
end

val all_paths : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) Path.t list
(** Returns all the paths leading to a value of type ['a] inside
    a value of type ['b]. Does not traverse list, array, lazy and objects.
    {e Will loop on recursive types.} *)

val project_path : 'a Ttype.t -> ('a, 'b) Path.t -> 'b Ttype.t
(** Extraction of sub-type pointed to by a path. *)
