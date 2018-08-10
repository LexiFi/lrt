open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "t"

let me = "[@@deriving t]"
let raise_str ?loc ?sub ?if_highlight (s : string) =
  Ppx_deriving.raise_errorf ?sub ?if_highlight ?loc "%s: %s" me s
let sprintf = Format.sprintf

let mangle_lid = Ppx_deriving.mangle_lid (`Suffix deriver)

let wrap_runtime decls =
  Ppx_deriving.sanitize ~module_:(Lident "Ppx_deriving_dynt_runtime") decls

type options = { abstract : label option ; path : label list }
let parse_options ~path options : options =
  let default = { abstract = None ; path } in
  List.fold_left (fun acc (name, expr) ->
    let loc = expr.pexp_loc in
      match name with
      | "abstract" ->
        let name = match expr.pexp_desc with
          | Pexp_constant (Pconst_string (name, None )) -> name
          | _ -> raise_str ~loc "please provide a string as abstract name"
        in  { acc with abstract = Some name }
      | _ -> raise_str ~loc
               ( sprintf "option %s not supported" name )
    ) default options

let rec stype_of_core_type t =
  let loc  = t.ptyp_loc in
  let fail () = raise_str ~loc "type not yet supported"
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

let stype_of_type_decl ~opt x =
  let loc = x.ptype_loc in
  match opt.abstract with
  | Some name -> let n = Const.string name |> Exp.constant in
    [%expr DT_abstract ([%e n], []) ]
  | None ->
    match x.ptype_kind with
    | Ptype_abstract -> begin match x.ptype_manifest with
        | None -> raise_errorf ~loc "no manifest found"
        | Some t -> stype_of_core_type t
      end
    | Ptype_record _
    | Ptype_open
    | Ptype_variant _ -> raise_str ~loc (sprintf "type kind not yet supported")

let str_of_type ~options ~path ({ ptype_loc = loc ; _} as type_decl) =
  let opt = parse_options ~path options in
  let decl = Ppx_deriving.mangle_type_decl (`Suffix deriver) type_decl in
  let ttype = [%expr [%e stype_of_type_decl ~opt type_decl] |> Obj.magic] in
  [Vb.mk (pvar decl) (wrap_runtime ttype)]

let sig_of_type ~options ~path ({ ptype_loc = loc ; _} as type_decl) =
  let _opt = parse_options ~path options in
  let typ  = Ppx_deriving.core_type_of_type_decl type_decl in
  let name = Ppx_deriving.mangle_type_decl (`Suffix deriver) type_decl in
  let typ =
    match type_decl.ptype_kind with
    | Ptype_abstract -> [%type: [%t typ] Dynt.Types.ttype ]
    | _ -> raise_str ~loc "cannot handle this type in signatures yet"
  in
  [Sig.value (Val.mk (mknoloc name) typ)]

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
