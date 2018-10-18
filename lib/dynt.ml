module Stype = Stype
module TypEq = TypEq
module Ttype = Ttype
module Xtype = Xtype
module Unify = Unify
module Matcher = Matcher
module Path = Path
module Print = Print
module Check = Check
module Variant = Variant
module Json = Json
module Std = Std

(**/**)
module Dynt_ppx_runtime = Dynt_ppx_runtime
(**/**)

(** {3 open Dynt}

    Modules using the dynt package should generally [open Dynt] at the toplevel.
*)

type stype = Stype.t
type 'a ttype = 'a Ttype.t
type 'a xtype = 'a Xtype.t
type dynamic = Ttype.dynamic = Dyn : 'a ttype * 'a -> dynamic

include Std
