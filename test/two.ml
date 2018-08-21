open Dynt

type public = int * int [@@deriving t]
type hidden = string * int [@@deriving t { abstract = "Two.hidden" }]
