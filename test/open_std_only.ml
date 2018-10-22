(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

open Dynt.Std

type t = int [@@abstract] [@@deriving t]

let p : (int array list, int) Dynt.Path.t = [%path? [[101]; [|42|]]]
let _ = p
