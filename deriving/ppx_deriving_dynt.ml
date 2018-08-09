open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "t"
let raise_errorf = Ppx_deriving.raise_errorf

let mangle_lid = Ppx_deriving.mangle_lid (`Suffix deriver)

let wrap_runtime decls =
  Ppx_deriving.sanitize ~module_:(Lident "Ppx_deriving_dynt_runtime") decls

let parse_options options =
  options |> List.iter (fun (name, expr) ->
      match name with
      | _ -> raise_errorf ~loc:expr.pexp_loc
               "%s does not support option %s" deriver name )

let rec stype_of_core_type t =
  let loc  = t.ptyp_loc in
  let fail () = raise_errorf ~loc "type not yet supported: %s"
           (Ppx_deriving.string_of_core_type t)
  in
  match t.ptyp_desc with
  | Ptyp_tuple l  ->
    let l = List.rev_map stype_of_core_type l in
    Ppx_deriving.fold_exprs (fun acc e ->
        [%expr (stype_of_ttype [%e e]) :: [%e acc]]) ([%expr [] ] :: l)
    |> fun x -> [%expr DT_tuple ([%e x])]
  | Ptyp_constr (id, []) ->
      let n = mangle_lid id.txt |>  Longident.flatten |> String.concat "."
              |> evar
      in
      [%expr [%e n]]
  | _ -> fail ()

let stype_of_type_decl x =
  let loc = x.ptype_loc in
  let t = Ppx_deriving.core_type_of_type_decl x in
  match x.ptype_kind with
  | Ptype_abstract -> begin match x.ptype_manifest with
      | None -> raise_errorf ~loc "no manifest found for type %s"
                  (Ppx_deriving.string_of_core_type t)
      | Some t -> stype_of_core_type t
    end
  | Ptype_record _
  | Ptype_open
  | Ptype_variant _ -> raise_errorf ~loc "type kind not yet supported: %s"
                  (Ppx_deriving.string_of_core_type t)

let str_of_type ~options ~path ({ ptype_loc = loc ; _} as type_decl) =
  ignore(path);
  parse_options options;
  let decl = Ppx_deriving.mangle_type_decl (`Suffix deriver) type_decl in
  let ttype = [%expr [%e stype_of_type_decl type_decl] |> Obj.magic] in
  [Vb.mk (pvar decl) (wrap_runtime ttype)]

let sig_of_type ~options ~path ({ ptype_loc = loc ; _} as type_decl) =
  ignore(path);
  parse_options options;
  let _typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let typ =
    match type_decl.ptype_kind with
    | _ -> raise_errorf ~loc "%s cannot handle signature yet" deriver
  in
  let decl = Ppx_deriving.mangle_type_decl (`Suffix deriver) type_decl in
  [Sig.value (Val.mk (mknoloc decl) typ)]

let () =
  let type_decl_str ~options ~path type_decls =
    List.map (str_of_type ~options ~path) type_decls
    |> List.concat
    |> Str.value Nonrecursive
    |> fun x -> [x]
  and type_decl_sig ~options ~path type_decls =
    List.map (sig_of_type ~options ~path) type_decls
    |> List.concat
  in
  Ppx_deriving.(register (create deriver ~type_decl_str ~type_decl_sig ()))
