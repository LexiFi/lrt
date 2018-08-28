module Types = Dynt_core.Types
module Primitives = Dynt_core.Primitives
module Path = Path
module Xtypes = Xtypes

module DynTools = struct
  module Print = Print
  module Check = Check
end

include Primitives

type 'a ttype = 'a Types.ttype
type ('a,'b) path = ('a,'b, Path.kind) Path.t
