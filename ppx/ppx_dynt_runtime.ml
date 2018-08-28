open Dynt_core
open Dynt_core.Internal

let ttype_of_stype (type a) (s : stype) = (Obj.magic s : a ttype)

let substitute = substitute
let stype_of_ttype = stype_of_ttype
let create_node = create_node
let set_node_record = set_node_record
let set_node_variant = set_node_variant
