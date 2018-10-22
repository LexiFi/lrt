open Dynt.Std

type t = int [@@abstract] [@@deriving t]

let p : (int array list, int) Dynt.Path.t = [%path? [[101]; [|42|]]]
let _ = p
