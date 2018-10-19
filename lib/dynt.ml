(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** Dynamic types for OCaml. *)

(** {3 Introduction}

    TODO: Pitch
*)

(** {3 Constructing dynamic types}

    TODO: Describe usage of [[@@deriving t]].

    Runtime representations of the basic OCaml types can be found in the {!Std}
    module. These definitions are generally required, when you use the
    [[@@deriving t]] syntax extension.
*)

module Std = Std

(** {3 Using dynamic types}

    TODO: Some words on the example applications.
*)

module Print = Print
module Variant = Variant
module Json = Json
module Check = Check

(** {3 Paths}

    We include an implementation of lenses and list of lenses: {!Path} enables
    access of values in nested tuples, records and constructors.  Additionally,
    paths can be used to access nested types (see {!Xtype.project_path}).

    Paths are constructed using the [[?path: .]] syntax extension.
*)

module Path = Path

(** {3 Type representation}

    Dynt comes with different representations of runtime types. Depending on the
    application, one might use one or another.

    {!Stype.t} or {!stype} in short are an untyped representation of dynamic
    types. Stypes are easy to construct, serializable and allow to write for
    unsafe but powerful code. Most users want to avoid this interface.

    {!Ttype.t} or {!ttype} in short extend the untyped {!stype} with an OCaml
    type. Ttypes are constructed by the [[@@deriving t]] syntax extension and
    can be used to safely consume APIs that make use of dynamic types.

    {!Xtype.t} enable safe inspection of dynamic types. Xtypes are used to
    implement APIs that make use of dynamic types.
*)

module Stype = Stype
module Ttype = Ttype
module Xtype = Xtype

(** {3 Unification and pattern matching}

    TODO: Describe Unify and Matcher
*)

module Unify = Unify
module Matcher = Matcher

(** {3 Type equality}

    Some of the other modules are able to check for type equality of dynamically
    crafted types. Such type equalities are inherently out of reach for the
    OCaml type system. They are "transported back" with help of the {!TypEq}
    module.

    A value of type [('a, 'b) TypEq.t] can be interpreted as equality proof for
    ['a] and ['b]. OCaml's type system accepts this proof when you open the
    GADT constructor {!TypEq.Eq}. An unwrap may look like the following.

    {[
      let plus: type a. int -> a -> (a, int) TypEq.t -> int =
       fun a b eq ->
        let TypEq.Eq = eq in
        a + b
    ]}
*)

module TypEq = TypEq

(** {3 open Dynt}

    Place [open Dynt] at the toplevel of your modules to have the dynamic
    representation of basic ocaml types available when you need them.
*)

type stype = Stype.t
type 'a ttype = 'a Ttype.t
type dynamic = Ttype.dynamic = Dyn : 'a ttype * 'a -> dynamic

include Std

(**/**)

module Dynt_ppx_runtime = Dynt_ppx_runtime

(**/**)
