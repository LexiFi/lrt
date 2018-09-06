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
      let _ : [%t f] -> [%t t] = begin [@ocaml.warning "-11"]
        function [%p  c] -> x | _ -> assert false
      end in
      (Constructor [%e estring ~loc (Longident.name lid.txt)]
       : ([%t f], [%t t]) step)
    ]
  (* Field *)
  | Pexp_ident lid ->
    let get = pexp_field ~loc (evar ~loc "x") lid in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = fun x -> [%e get] in
      (Field [%e estring ~loc (Longident.name lid.txt)]
       : ([%t f], [%t t]) step)]
  (* Tuple *)
  | Pexp_apply ({pexp_desc = Pexp_ident {txt=Lident "t";_}
                ; pexp_loc = loc; _}, args) ->
    let pat = Ast_pattern.( __ ** eint __ ^:: __ ** eint __ ^:: nil) in
    let nth, arity = Ast_pattern.parse pat loc args (fun _ a _ b -> (a,b)) in
    let () = if not (nth < arity) || nth < 0 || arity < 2 then
        raise_errorf ~loc "Specify tuple step with t <nth> <arity>" in
    let arr = Array.make arity (ppat_any ~loc) in
    let () = Array.set arr nth (ppat_var ~loc {txt="x";loc}) in
    let p = ppat_tuple ~loc (Array.to_list arr) in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = function [%p p] -> x in
      (Tuple [%e eint ~loc nth]
       : ([%t f], [%t t]) step)]
  (* List *)
  | Pexp_apply ({pexp_desc = Pexp_ident {txt=Lident "l";_}
                ; pexp_loc = loc; _}, args) ->
    let pat = Ast_pattern.( __ ** eint __ ^:: nil) in
    let nth = Ast_pattern.parse pat loc args (fun _ a -> a) in
    let () = if nth < 0 then
        raise_errorf ~loc "Specify list step with l <nth>" in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = fun l -> List.nth l [%e eint ~loc nth] in
      (List [%e eint ~loc nth]
       : ([%t f], [%t t]) step)]
  (* array *)
  | Pexp_apply ({pexp_desc = Pexp_ident {txt=Lident "a";_}
                ; pexp_loc = loc; _}, args) ->
    let pat = Ast_pattern.( __ ** eint __ ^:: nil) in
    let nth = Ast_pattern.parse pat loc args (fun _ a -> a) in
    let () = if nth < 0 then
        raise_errorf ~loc "Specify list step with l <nth>" in
    let f, t = v#pair loc in
    [%expr
      let _ : [%t f] -> [%t t] = fun a -> Array.get a [%e eint ~loc nth] in
      (Array [%e eint ~loc nth]
       : ([%t f], [%t t]) step)]
  | _ ->
    raise_errorf "Cannot make sense of this step" ~loc

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
