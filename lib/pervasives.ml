open Types

(* TODO: what is your actual representation of the following types? *)
let unit_t = DT_abstract ("unit", []) |> Obj.magic
let bool_t = DT_abstract ("bool", []) |> Obj.magic
let char_t = DT_abstract ("char", []) |> Obj.magic
let nativeint_t = DT_abstract ("nativeint", []) |> Obj.magic
let int32_t = DT_abstract ("int32", []) |> Obj.magic
let int64_t = DT_abstract ("int64", []) |> Obj.magic

let int_t = DT_int |> Obj.magic
let string_t = DT_string |> Obj.magic
let float_t = DT_float |> Obj.magic

let option_t a =
  let t = stype_of_ttype a in
  DT_option t |> Obj.magic

let list_t a =
  let t = stype_of_ttype a in
  DT_list t |> Obj.magic

let array_t a =
  let t = stype_of_ttype a in
  DT_array t |> Obj.magic

let lazy_t_t a =
  let t = stype_of_ttype a in
  DT_abstract ("Lazy.t", [t]) |> Obj.magic
