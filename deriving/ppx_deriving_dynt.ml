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

(* Name of the stype, used in recursive type definitions*)
let rec_stype_label="__rec_stype"

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
let rec str_of_core_type ~opt ~recurse ({ ptyp_loc = loc ; _ } as ct) =
  let fail () = raise_str ~loc "type not yet supported" in
  let rc = str_of_core_type ~opt ~recurse in
  let t = match ct.ptyp_desc with
    | Ptyp_tuple l ->
      let args = List.rev_map rc l |> List.fold_left (fun acc e ->
          [%expr stype_of_ttype [%e e] :: [%e acc]]) [%expr []]
      in
      [%expr Obj.magic (DT_tuple [%e args])]
    | Ptyp_constr (id, args) ->
      if id.txt = Lident recurse then
        [%expr Obj.magic [%e evar rec_stype_label]]
      else
        let id' = { id with txt = mangle_lid id.txt} in
        List.fold_left
          (fun acc e -> [%expr [%e acc] [%e rc e]])
          [%expr [%e Exp.ident id']] args
    | Ptyp_var vname -> [%expr [%e evar vname]]
    | _ -> fail ()
  in
  match opt.abstract with
  | Some name ->
    [%expr Obj.magic( DT_abstract ([%e str name],[]))]
  | None -> t

(* Construct record ttypes *)
let str_of_record_labels ?inline ~loc ~opt ~name ~recurse l =
  let ll = List.rev_map (fun {pld_loc = loc; pld_name; pld_type; _ } ->
      let t = str_of_core_type ~opt ~recurse pld_type in
      [%expr ([%e str pld_name.txt], [], stype_of_ttype [%e t])]
    ) l |> expr_list ~loc
  in
  match inline with
  | None ->
    [%expr Internal.create_record_type [%e str name] []
        (fun [%p pvar rec_stype_label] -> [%e ll], Record_regular)
           |> Obj.magic ]
  | Some i ->
    [%expr Internal.create_record_type [%e str name] []
        (fun _ -> [%e ll], Record_inline [%e int i])
           |> Obj.magic ]

(* Construct variant ttypes *)
let str_of_variant_constructors ~loc ~opt ~name ~recurse l =
  let nconst_tag = ref 0 in
  let ll = List.rev_map (fun {pcd_loc = loc; pcd_name; pcd_args; _ } ->
      match pcd_args with
      | Pcstr_tuple ctl ->
        if ctl <> [] then incr nconst_tag;
        let l = List.rev_map (fun ct ->
            str_of_core_type ~opt ~recurse ct
            |> fun e -> [%expr stype_of_ttype [%e e]]
          ) ctl in
        [%expr ([%e str pcd_name.txt], [], C_tuple [%e expr_list ~loc l])]
      | Pcstr_record lbl ->
        let r =
          str_of_record_labels ~inline:!nconst_tag ~recurse
            ~opt ~loc ~name:(sprintf "%s.%s" name pcd_name.txt) lbl
        in
        incr nconst_tag;
        [%expr ([%e str pcd_name.txt], [], C_inline [%e r])]
    ) l |> expr_list ~loc
  in
  [%expr Internal.create_variant_type [%e str name] []
      (fun [%p pvar rec_stype_label] -> [%e ll]) |> Obj.magic ]

let free_vars_of_type_decl td =
  List.rev_map (fun (ct, _variance) ->
      match ct.ptyp_desc with
      | Ptyp_var name -> name
      | _ -> raise_str "type parameter not yet supported"
    ) td.ptype_params

(* Type declarations in structure.  Builds e.g.
 * let <type>_t : (<a> * <b>) ttype = pair <b>_t <a>_t
 *)
let str_of_type_decl ~options ~path ({ ptype_loc = loc ; _} as td) =
  let opt = parse_options ~path options in
  let name = td.ptype_name.txt in
  let recurse = name in
  let t = match td.ptype_kind with
    | Ptype_abstract -> begin match td.ptype_manifest with
        | None -> raise_errorf ~loc "no manifest found"
        | Some ct -> str_of_core_type ~opt ~recurse ct
      end
    | Ptype_variant l ->
      str_of_variant_constructors ~loc ~opt ~name ~recurse l
    | Ptype_record l -> str_of_record_labels ~loc ~opt ~name ~recurse l
    | Ptype_open ->
      raise_str ~loc "type kind not yet supported"
  in
  let e = List.fold_left
      (fun acc name -> lam (pvar name) acc)
      (wrap_runtime t) (free_vars_of_type_decl td)
  in
  let mangled = mangle_type_decl td in
  [Vb.mk (pvar mangled) e]


(* Type declarations in signature. Generates
 * val <type>_t : <type> ttype
 *)
let sig_of_type_decl ~options ~path ({ ptype_loc = loc ; _} as td) =
  let _opt = parse_options ~path options in
  let ct  = Ppx_deriving.core_type_of_type_decl td in
  let name = mangle_type_decl td in
  let typ =
    match td.ptype_kind with
    | Ptype_abstract
    | Ptype_record _
    | Ptype_variant _ -> [%type: [%t ct] Dynt.Types.ttype ]
    | _ -> raise_str ~loc "cannot handle this type in signatures yet"
  in
  let e = List.fold_left
      (fun acc name ->
         [%type: [%t Typ.var name] Dynt.Types.ttype -> [%t acc] ]) typ
      (free_vars_of_type_decl td)
  in
  [Sig.value (Val.mk (mknoloc name) e)]

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
