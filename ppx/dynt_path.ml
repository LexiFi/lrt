(** The [\[%path . \]] syntax extension. *)

(**/**)

open Ppxlib
(* open Ast_builder.Default *)

let expression ~loc ~path x =
  ignore loc; ignore path; x

(* Register the generator functions *)
let () =
  let extensions =
    [ Extension.(declare "path" Context.expression
                   Ast_pattern.(single_expr_payload __)) expression ] in
  Driver.register_transformation "dynt_path" ~extensions
