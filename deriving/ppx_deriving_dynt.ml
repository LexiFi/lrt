open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

(* Who are we? *)
let deriver = "t"
let me = "[@@deriving t]"

(* How are names derived? We use suffix over prefix *)
let mangle_lid = Ppx_deriving.mangle_lid (`Suffix deriver)
let mangle_type_decl = Ppx_deriving.mangle_type_decl (`Suffix deriver)

(* Make accesible the runtime module at runtime *)
let wrap_runtime decls =
  Ppx_deriving.sanitize ~module_:(Lident "Ppx_deriving_dynt_runtime") decls

(* Helpers for error raising *)
let raise_str ?loc ?sub ?if_highlight (s : string) =
  Ppx_deriving.raise_errorf ?sub ?if_highlight ?loc "%s: %s" me s
let sprintf = Format.sprintf

(* More helpers *)
let expand_path = Ppx_deriving.expand_path (* this should mix in library name
                                              at some point *)

(* Combine multiple expressions into a list expression *)
let expr_list ~loc lst =
  Ppx_deriving.fold_exprs (fun acc el ->
      [%expr [%e el] :: [%e acc]]) ([%expr []] :: lst)

(* read options from e.g. [%deriving t { abstract = "Hashtbl.t" }] *)
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

(* Construct ttype generator from core type *)
let rec str_of_core_type ~opt ({ ptyp_loc = loc ; _ } as ct) =
  let fail () = raise_str ~loc "type not yet supported" in
  let rc = str_of_core_type ~opt in
  let t = match ct.ptyp_desc with
    | Ptyp_tuple []
    | Ptyp_tuple [_] -> raise_str ~loc "tuple too small"
    | Ptyp_tuple [a; b] ->
      let a, b = rc a, rc b in
      [%expr pair ([%e a]) [%e b]]
    | Ptyp_tuple [a; b; c] ->
      let a, b, c = rc a, rc b, rc c in
      [%expr triple ([%e a]) [%e b] [%e c]]
    | Ptyp_tuple [a; b; c; d] ->
      let a, b, c, d = rc a, rc b, rc c, rc d in
      [%expr quartet ([%e a]) [%e b] [%e c] [%e d]]
    | Ptyp_tuple [a; b; c; d; e] ->
      let a, b, c, d, e = rc a, rc b, rc c, rc d, rc e in
      [%expr quintet ([%e a]) [%e b] [%e c] [%e d] [%e e]]
    | Ptyp_tuple _ -> raise_str ~loc "tuple too big"
    | Ptyp_constr (id, []) ->
      let id' = { id with txt = mangle_lid id.txt} in
      [%expr [%e Exp.ident id']]
    | _ -> fail ()
  in
  match opt.abstract with
  | Some name ->
    let n = Const.string name |> Exp.constant in
    [%expr make_abstract ~name:[%e n] [%e t]]
  | None -> t

(* Construct record ttypes *)
let str_of_record_labels ?inline ~loc ~opt ~name l =
  let ll = List.rev_map (fun {pld_loc = loc; pld_name; pld_type; _ } ->
      let t = str_of_core_type ~opt pld_type in
      let name = Const.string pld_name.txt |> Exp.constant in
      [%expr make_record_field ~name:[%e name] (stype_of_ttype [%e t])]
    ) l |> expr_list ~loc
  in
  let name = Const.string name |> Exp.constant in
  match inline with
  | None ->
    [%expr make_record ~name:[%e name] [] (fun _ -> [%e ll]) |> Obj.magic ]
  | Some i ->
    let i = Const.int i |> Exp.constant in
    [%expr make_record ~name:[%e name] ~inline:[%e i] []
        (fun _ -> [%e ll]) ]

(* Construct variant ttypes *)
let str_of_variant_constructors ~loc ~opt ~name l =
  let nconst_tag = ref 0 in
  let ll = List.rev_map (fun {pcd_loc = loc; pcd_name; pcd_args; _ } ->
      let nameexp = Const.string pcd_name.txt |> Exp.constant in
      match pcd_args with
      | Pcstr_tuple ctl ->
        if ctl <> [] then incr nconst_tag;
        let l = List.rev_map (fun ct ->
            str_of_core_type ~opt ct
            |> fun e -> [%expr stype_of_ttype [%e e]]
          ) ctl in
        [%expr make_variant_constructor_tuple ~name:[%e nameexp]
            [%e expr_list ~loc l]]
      | Pcstr_record lbl ->
        let r =
          str_of_record_labels ~inline:!nconst_tag
            ~opt ~loc ~name:(sprintf "%s.%s" name pcd_name.txt) lbl
        in
        incr nconst_tag;
        [%expr make_variant_constructor_inline ~name:[%e nameexp] [%e r]]
    ) l |> expr_list ~loc
  in
  let name = Const.string name |> Exp.constant in
  [%expr make_variant ~name:[%e name] [] (fun _ -> [%e ll]) |> Obj.magic ]

(* Type declarations in structure.  Builds e.g.
 * let <type>_t : (<a> * <b>) ttype = pair <b>_t <a>_t
 *)
let str_of_type_decl ~options ~path
    ({ ptype_loc = loc ; ptype_name ; _} as td) =
  let opt = parse_options ~path options in
  let name = ptype_name.txt in
  let t = match td.ptype_kind with
    | Ptype_abstract -> begin match td.ptype_manifest with
        | None -> raise_errorf ~loc "no manifest found"
        | Some ct -> str_of_core_type ~opt ct
      end
    | Ptype_variant l -> str_of_variant_constructors ~loc ~opt ~name l
    | Ptype_record l -> str_of_record_labels ~loc ~opt ~name l
    | Ptype_open ->
      raise_str ~loc (sprintf "type kind not yet supported")
  in
  let name = mangle_type_decl td in
  [Vb.mk (pvar name) (wrap_runtime [%expr [%e t]])]

(* Type declarations in signature. Generates
 * val <type>_t : <type> ttype
 *)
let sig_of_type_decl ~options ~path ({ ptype_loc = loc ; _} as td) =
  let _opt = parse_options ~path options in
  let ct  = Ppx_deriving.core_type_of_type_decl td in
  let name = mangle_type_decl td in
  let typ =
    match td.ptype_kind with
    | Ptype_abstract -> [%type: [%t ct] Dynt.Types.ttype ]
    | _ -> raise_str ~loc "cannot handle this type in signatures yet"
  in
  [Sig.value (Val.mk (mknoloc name) typ)]

(* Register the handler for type declarations in signatures and structures *)
let () =
  let type_decl_str ~options ~path type_decls =
    List.map (str_of_type_decl ~options ~path) type_decls
    |> List.concat
    |> Str.value Nonrecursive
    |> fun x -> [x]
  and type_decl_sig ~options ~path type_decls =
    List.map (sig_of_type_decl ~options ~path) type_decls
    |> List.concat
  in
  Ppx_deriving.(register (create deriver ~type_decl_str ~type_decl_sig ()))
