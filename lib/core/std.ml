open Ttype
open Stype

(* CAUTION: This must be consistent with xtype_of_ttype *)
let unit_t = DT_abstract ("unit", []) |> Obj.magic
let bool_t = DT_abstract ("bool", []) |> Obj.magic
let char_t = DT_abstract ("char", []) |> Obj.magic
let int32_t = DT_abstract ("int32", []) |> Obj.magic
let int64_t = DT_abstract ("int64", []) |> Obj.magic
let nativeint_t = DT_abstract ("nativeint", []) |> Obj.magic

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

let ttype_t a =
  let t = stype_of_ttype a in
  DT_abstract ("Dynt.Types.ttype", [t]) |> Obj.magic

let lazy_t a =
  let t = stype_of_ttype a in
  DT_abstract ("Lazy.t", [t]) |> Obj.magic

let hashtbl_t a b =
  let a = stype_of_ttype a in
  let b = stype_of_ttype b in
  DT_abstract ("Hashtbl.t", [a;b]) |> Obj.magic
