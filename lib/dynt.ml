module Types = Types
module Path = Path
module Xtypes = Xtypes
module DynPrint = Print
module Primitives = Primitives

include Primitives

type 'a ttype = 'a Types.ttype
type ('a,'b) path = ('a,'b, Path.kind) Path.t
