include Ppx_deriving_runtime
include Dynt.Pervasives
include Dynt.Types
include Internal

(* let ttype_of_stype (type a) (s : stype) = (Obj.magic s : a ttype) *)
let ttype_of_stype (s : stype) = Obj.magic s

let set_record descr t =
  match stype_of_ttype t with
  | DT_node n -> set_node_record n descr
  | _ -> assert false

let set_variant descr t =
  match stype_of_ttype t with
  | DT_node n -> set_node_variant n descr
  | _ -> assert false
