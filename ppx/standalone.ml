(** Standalone PPX rewriter

    Use [dune exec ppx/standalone.exe source.ml] to see the ppx generated output
    of [soucee.ml]
*)

open Ppxlib

let () = Driver.standalone ()
