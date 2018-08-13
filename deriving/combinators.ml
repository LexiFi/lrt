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

type record_field = (string * stype_properties * stype)

let make_record_field ~name stype =
  (name, [], stype)

let make_record ~name ?inline _stypes stype_to_fields =
  let r = match inline with
    | None -> Record_regular
    | Some i -> Record_inline i
  in
  let f x = stype_to_fields x , r in
  Internal.create_record_type name [] f

type variant_constructor = (string * stype_properties * stype variant_args)

let make_variant_constructor_tuple ~name stypes =
  (name, [], C_tuple stypes)

let make_variant_constructor_inline ~name stype =
  (name, [], C_inline stype)

let make_variant ~name _stypes stype_to_constructors =
  Internal.create_variant_type name [] stype_to_constructors
