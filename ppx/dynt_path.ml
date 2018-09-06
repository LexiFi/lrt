(** The [\[%path . \]] syntax extension. *)

(**/**)

open Ppxlib
open Ast_builder.Default

let raise_errorf ~loc =
  Format.ksprintf (Location.raise_errorf ~loc "%s %s" "[%path .]")

class tyvar = object
  val mutable i = 0

  method pair loc =
    i <- i + 1 ;
    ( Printf.sprintf "t%d" (i-1) |> ptyp_var ~loc
    , Printf.sprintf "t%d" i |> ptyp_var ~loc)
end

let rec expand_single v ({pexp_loc = loc; _} as x) =
  match x.pexp_desc with
  (* Constructor *)
  | Pexp_construct (lid, None) ->
    let p = Some (ppat_var ~loc {txt="x";loc}) in
    let c = ppat_construct ~loc lid p in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = (function [%p  c] -> x | _ -> assert false ) in
      (Constructor [%e estring ~loc (Longident.name lid.txt)]
       : ([%t f], [%t t]) step)
    ]
  (* Field *)
  | Pexp_ident _lid -> x
  | _ -> raise_errorf "This should be a single step" ~loc
and expand v acc x =
  match x.pexp_desc with
  (* Concatenation *)
  | Pexp_sequence (hd, tl) -> expand v (expand_single v hd :: acc) tl
  (* Single step *)
  | _ -> expand_single v x :: acc

let expand ~loc ~path x =
  ignore path;
  let v = new tyvar in
  [%expr let open P in [%e elist ~loc (expand v [] x |> List.rev)]]

(* Register the expander *)
let () =
  let extensions =
    [ Extension.(declare "path" Context.expression
                   Ast_pattern.(single_expr_payload __)) expand ] in
  Driver.register_transformation "dynt_path" ~extensions
