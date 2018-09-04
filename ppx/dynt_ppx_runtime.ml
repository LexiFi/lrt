open Dynt_internal
open Dynt_internal.Internal

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

module Set = Set.Make(String)
let abstract_names = ref Set.empty

exception Non_unique_abstract_name of string

let register_abstract_name s =
  if Set.mem s !abstract_names then raise (Non_unique_abstract_name s);
  abstract_names := Set.add s !abstract_names
