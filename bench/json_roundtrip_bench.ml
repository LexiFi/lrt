open Dynt

type t = Variant_roundtrip_type.t list [@@deriving t]

let filename =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else (
    Printf.eprintf "Please provide filename of data as first argument" ;
    exit 1 )

let value = Variant.value_of_variant_in_file ~t filename

(*
type 'a tree =
  | L of 'a
  | N of 'a tree * 'a tree
[@@deriving t]

let tree_to_json = fun a_t -> Json.to_json (tree_t a_t)

let[@landmark "1_leave"] () =
  for _i = 1 to 100000 do
    ignore (tree_to_json int_t (L 1))
  done
let[@landmark "2_leaves"] () =
  for _i = 1 to 100000 do
    ignore (tree_to_json int_t (N (L 1, L 2)))
  done
let[@landmark "3_leaves"] () =
  for _i = 1 to 100000 do
    ignore (tree_to_json int_t (N (L 1, N (L 2, L 3))))
  done
*)

let[@landmark "prepare"] of_json, to_json = (Json.of_json t, Json.to_json t)

let[@landmark "iteration"] run () =
  let[@landmark "to_json"] json = to_json value in
  let[@landmark "of_json"] value' = of_json json in
  ignore value'

let _ = List.init 10 (fun _ -> run ())
