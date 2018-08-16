include Ppx_deriving_runtime
include Dynt.Pervasives
include Dynt.Types

(* let ttype_of_stype (type a) (s : stype) = (Obj.magic s : a ttype) *)
let ttype_of_stype (s : stype) = Obj.magic s
