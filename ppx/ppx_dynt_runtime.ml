open Dynt_core
open Dynt_core.Internal

type 'a lazy_t = 'a Lazy.t
type nonrec 'a ttype = 'a ttype
type nonrec node = node

let ttype_of_stype (type a) (s : stype) = (Obj.magic s : a ttype)

let substitute = substitute
let stype_of_ttype = stype_of_ttype
let create_node = create_node
let set_node_record = set_node_record
let set_node_variant = set_node_variant

let rev_map2 = List.rev_map2
let force = Lazy.force

let record_representation (l: stype list) : record_repr =
  let p = types_equality_modulo_props (stype_of_ttype float_t) in
  if List.for_all p l then Record_float else Record_regular
