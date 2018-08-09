open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "dynt"
let raise_errorf = Ppx_deriving.raise_errorf

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc
             "%s does not support option %s" deriver name )

let str_of_type ~options ~path ({ ptype_loc = loc ; _} as type_decl) =
  ignore(path);
  parse_options options;
  let quoter = Ppx_deriving.create_quoter () in
  let creator =
    match type_decl.ptype_kind with
    | _ -> [%expr DT_tuple [DT_int;DT_int] |> Obj.magic ]
  in
  let decl = Ppx_deriving.mangle_type_decl (`Suffix deriver) type_decl in
  [Vb.mk (pvar decl) (Ppx_deriving.sanitize ~quoter creator)]

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
