open Types

let int_t = DT_int |> Obj.magic
let string_t = DT_string |> Obj.magic
let float_t = DT_float |> Obj.magic

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
