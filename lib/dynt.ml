open Dynt_core

module Stype = Stype
module Ttype = struct
  include Ttype
  let t = Std.ttype_t
end
module TypEq = TypEq
module Path = Path

module Xtype = Xtype
module Print = Print
module Check = Check
module Variant = Variant

include Std
