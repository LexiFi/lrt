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

type stype = Stype.t
type 'a ttype = 'a Ttype.t
type 'a xtype = 'a Xtype.t
type dynamic = Ttype.dynamic = Dyn : 'a ttype * 'a -> dynamic

include Std

(**/**)

module Dynt_ppx_runtime = Dynt_ppx_runtime
