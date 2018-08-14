open Types

let int_t = DT_int |> Obj.magic
let string_t = DT_string |> Obj.magic
let float_t = DT_float |> Obj.magic

let option_t a =
  let t = stype_of_ttype a in
  DT_option t |> Obj.magic

let list_t a =
  let t = stype_of_ttype a in
  DT_list t |> Obj.magic
