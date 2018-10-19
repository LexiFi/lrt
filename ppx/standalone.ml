(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** Standalone PPX rewriter

    Use [dune exec ppx/standalone.exe source.ml] to see the ppx generated output
    of [source.ml]
*)

open Ppxlib

let () = Driver.standalone ()
