open Dynt.Types

let dg = stype_of_ttype

let pair a b =
  let a, b = dg a, dg b in
  DT_tuple ([a; b]) |> Obj.magic

let triple a b c =
  let a, b, c = dg a, dg b, dg c in
  DT_tuple ([a; b; c]) |> Obj.magic

let quartet a b c d =
  let a, b, c, d = dg a, dg b, dg c, dg d in
  DT_tuple ([a; b; c; d]) |> Obj.magic

let quintet a b c d e =
  let a, b, c, d, e = dg a, dg b, dg c, dg d, dg e in
  DT_tuple ([a; b; c; d; e]) |> Obj.magic

let make_abstract ~name _t =
  DT_abstract (name, []) |> Obj.magic

type variant_constructor = (string * stype_properties * stype variant_args)

let make_variant_constructor ~name stypes =
  (name, [], C_tuple stypes)

let make_variant ~name _stypes stype_to_constructors =
  Internal.create_variant_type name [] stype_to_constructors
