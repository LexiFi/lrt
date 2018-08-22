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
let mangle_type_decl ?(n=deriver) =
  Ppx_deriving.mangle_type_decl (`Suffix n)

type names = { typ : label ; ttype : label; node : label}
let names_of_type_decl td =
  { typ = td.ptype_name.txt;
    ttype = mangle_type_decl td;
    node = mangle_type_decl ~n:"node" td
  }

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

(* Deconstruct expression to string. Fail otherwise *)
let string_of_expr ~loc expr =
  match expr.pexp_desc with
  | Pexp_constant (Pconst_string (name, None )) -> name
  | _ -> raise_str ~loc "this should be a string expression"

(* read options from e.g. [%deriving t { abstract = "Hashtbl.t" }] *)
type options = { abstract : label option ; path : label list }
let parse_options ~path options : options =
  let default = { abstract = None ; path } in
  List.fold_left (fun acc (name, expr) ->
    let loc = expr.pexp_loc in
      match name with
      | "abstract" ->
        let name = string_of_expr ~loc expr in
        { acc with abstract = Some name }
      | _ -> raise_str ~loc
               ( sprintf "option %s not supported" name )
    ) default options

let find_index_opt (l : 'a list) (el : 'a) : int option =
  let i = ref 0 in
  let rec f = function
    | [] -> None
    | hd :: _ when hd = el -> Some !i
    | _ :: tl -> incr i ; f tl
  in f l

(* helpers for lazy recursion *)

let lazy_value_binding ~loc name basetyp expr =
  let pat = Pat.constraint_ (pvar name) [%type: [%t basetyp] Lazy.t] in
  let expr = [%expr lazy [%e expr]] in
  Vb.mk pat expr

let force_lazy ~loc var = [%expr Lazy.force [%e var]]

(* extract properties = (string * string) list from attribute list *)
let properties_of_attributes attrs =
  List.map (fun ({txt; loc},_) ->
      match Ppx_deriving.attr ~deriver txt attrs
            |> Ppx_deriving.Arg.(get_attr ~deriver string) with
      | Some v -> tuple [str txt; str v]
      | None -> raise_str ~loc "internal error (properties_of_attributes)"
    ) attrs

(* Construct ttype expression from core type *)
let rec core_type ~opt ~rec_ ~free ({ ptyp_loc = loc ; _ } as ct) =
  let rc = core_type ~opt ~rec_ ~free in
  let rcs ct = [%expr stype_of_ttype [%e rc ct]] in
  let t = match ct.ptyp_desc with
    | Ptyp_tuple l ->
      let args = List.map rcs l in
      [%expr ttype_of_stype (DT_tuple [%e list args])]
    | Ptyp_constr (id, args) -> begin
        let id' = { id with txt = mangle_lid id.txt} in
        (* recursive identifier? regular recursion? *)
        match List.assoc_opt (Longident.last id.txt) rec_ with
        | Some l ->
          let is = List.rev_map (fun x -> x.ptyp_desc) args
          and should = List.rev_map (fun a -> Ptyp_var a) l in
          if is = should then
            Exp.ident id' |> force_lazy ~loc
          else
            raise_str ~loc "non-regular type recursion not supported"
        | None -> app (Exp.ident id') (List.map rc args)
      end
    | Ptyp_var vname -> begin
        match find_index_opt free vname with
        | None -> raise_str ~loc "invalid use of type variable"
        | Some i -> [%expr ttype_of_stype (DT_var [%e int i])]
      end
    | Ptyp_arrow (label, l, r) ->
      let lab =
        match label with
        | Nolabel -> ""
        | Labelled s -> s
          (* TODO: How do you actually represent optional arguments? *)
        | Optional s -> "?" ^ s
      in
      [%expr ttype_of_stype (DT_arrow ([%e str lab],[%e rcs l],[%e rcs r]))]
    (* TODO: is the closed flag relevant? *)
    | Ptyp_object (l, _closed_flag) ->
      let fields = List.map (function
          | Oinherit _ct -> raise_str ~loc "type not yet supported"
          | Otag ({txt; _}, _attr, ct) -> tuple [str txt; rcs ct]) l
      in
      [%expr ttype_of_stype (DT_object [%e list fields])]
    | _ -> raise_str ~loc "type not yet supported"
  in
  let with_prop =
    match properties_of_attributes ct.ptyp_attributes with
    | [] -> t
    | l -> [%expr
      ttype_of_stype (DT_prop ([%e list l], stype_of_ttype [%e t]))]
  in
  (* TODO: This is weird. We construct the ttype but do not use it.
   * TODO: Also, what about free variables? *)
  match opt.abstract with
  | Some name ->
    [%expr ttype_of_stype( DT_abstract ([%e str name],[]))]
  | None -> with_prop

let stypes_of_free ~loc free =
  List.mapi (fun i _v -> [%expr DT_var [%e int i]]) free |> list

(* Construct record ttypes *)
let fields_of_record_labels ~opt ~rec_ ~free l =
  List.map (fun ({pld_loc = loc; _ } as x) ->
      let props = properties_of_attributes x.pld_attributes in
      let t = core_type ~opt ~rec_ ~free x.pld_type in
      [%expr
        ([%e str x.pld_name.txt], [%e list props], stype_of_ttype [%e t])]
    ) l

let single_let_in pat expr =
  let_in [Vb.mk pat expr]

let record_labels ~loc ~opt ~me ~free ~rec_ l =
  let fields = fields_of_record_labels ~opt ~free ~rec_ l in
  let createnode = single_let_in (pvar me.node)
      [%expr create_node [%e str me.typ] [%e stypes_of_free ~loc free]]
  and ttype = [%expr ttype_of_stype (DT_node [%e evar me.node])]
  and setnode = single_let_in (punit ())
      [%expr
        set_node_record [%e evar me.node] ([%e list fields], Record_regular) ]
  in
  createnode, ttype, setnode

let record_labels_inline ~loc ~opt ~free ~rec_ ~name i l =
  let fields = fields_of_record_labels ~opt ~free ~rec_ l in
  [%expr
    let [%p pvar "inline_node"] : Dynt.Types.node =
      create_node [%e str name] [%e stypes_of_free ~loc free]
    in
    set_node_record [%e evar "inline_node"]
      ([%e list fields], Record_inline [%e int i]);
    DT_node [%e evar "inline_node"]]

(* Construct variant ttypes *)
let variant_constructors ~loc ~opt ~me ~free ~rec_ l =
  let nconst_tag = ref 0 in
  let constructors =
    List.map (fun ({pcd_loc = loc; _ } as x) ->
        let props = properties_of_attributes x.pcd_attributes in
        match x.pcd_args with
        | Pcstr_tuple ctl ->
          if ctl <> [] then incr nconst_tag;
          let l = List.rev_map (fun ct ->
              core_type ~opt ~rec_ ~free ct
              |> fun e -> [%expr stype_of_ttype [%e e]]
            ) ctl in
          [%expr ([%e str x.pcd_name.txt], [%e list props],
                  C_tuple [%e expr_list ~loc l])]
        | Pcstr_record lbl ->
          let name = sprintf "%s.%s" me.typ x.pcd_name.txt in
          let r = record_labels_inline ~rec_ ~free ~opt ~loc
              ~name !nconst_tag lbl
          in
          incr nconst_tag;
          [%expr ([%e str x.pcd_name.txt], [%e list props], C_inline [%e r])]
      ) l
  in
  let createnode = single_let_in (pvar me.node)
      [%expr create_node [%e str me.typ] [%e stypes_of_free ~loc free]]
  and ttype = [%expr ttype_of_stype (DT_node [%e evar me.node])]
  and setnode = single_let_in (punit ())
      [%expr
        set_node_variant [%e evar me.node] [%e list constructors] ]
  in
  createnode, ttype, setnode

let free_vars_of_type_decl td =
  List.map (fun (ct, _variance) ->
      match ct.ptyp_desc with
      | Ptyp_var name -> name
      | _ -> raise_str "type parameter not yet supported"
    ) td.ptype_params

(* generate type expressions of the form 'a list ttype *)
let basetyp_of_type_decl ~loc td =
  let ct  = Ppx_deriving.core_type_of_type_decl td in
  [%type: [%t ct] Dynt.Types.ttype]

(* generate type expresseion of the form 'a ttype -> 'a list ttype *)
let typ_of_free_vars ~loc ~basetyp free =
  List.fold_left (fun acc name ->
      [%type: [%t Typ.var name] Dynt.Types.ttype -> [%t acc]])
    basetyp (List.rev free)

let substitution_of_free_vars ~loc ~me basetyp free =
  let typ = typ_of_free_vars ~loc ~basetyp free in
  let subst =
    let arr = List.map (fun v ->
        [%expr stype_of_ttype [%e evar v]]) free
    in
    List.fold_left (fun acc v -> lam (pvar v) acc)
      [%expr ttype_of_stype (
          substitute [%e Exp.array arr]
            (stype_of_ttype [%e evar me.ttype]))]
      (List.rev free)
  in
  Vb.mk (Pat.constraint_ (pvar me.ttype) typ) (wrap_runtime subst)

(* alist mapping recursive identifiers to their type args *)
type recargs = (label * label list) list

let type_decl_str ~options ~path tds =
  let extend_let new_ was expr = new_ (was expr) in
  let opt = parse_options ~path options in
  let rec_ : recargs = List.map (fun td ->
      td.ptype_name.txt, free_vars_of_type_decl td) tds
  in
  let parse (pats, cn, lr, sn, fl, subs) ({ ptype_loc = loc ; _} as td) =
    let me = names_of_type_decl td in
    let basetyp = basetyp_of_type_decl ~loc td in
    let free = free_vars_of_type_decl td in
    let pats = (Pat.constraint_ (pvar me.ttype) basetyp) :: pats in
    let cn, ttype, sn =
      match td.ptype_kind with
      | Ptype_abstract -> begin match td.ptype_manifest with
          | None -> raise_errorf ~loc "no manifest found"
          | Some ct ->
            let t = core_type ~rec_ ~opt ~free ct in
            cn, t, sn
        end
      | Ptype_record l ->
        let c, t, s = record_labels ~me ~free ~loc ~opt ~rec_ l in
        extend_let c cn, t, extend_let s sn
      | Ptype_variant l ->
        let c, t, s = variant_constructors ~me ~free ~loc ~opt ~rec_ l in
        extend_let c cn, t, extend_let s sn
      | Ptype_open ->
        raise_str ~loc "type kind not yet supported"
    in
    let lr = lazy_value_binding ~loc me.ttype basetyp ttype :: lr in
    let fl = force_lazy ~loc (evar me.ttype) :: fl in
    let subs = if free = [] then subs else
        (substitution_of_free_vars ~loc ~me basetyp free) :: subs in
    pats, cn, lr, sn, fl, subs
  in
  let patterns, createnode, lazyrec, setnode, forcelazy, substitutions =
    let id = fun x -> x in
    List.fold_left parse ([], id ,[], id,[],[]) tds in
  let prepare =
    let pattern, force =
      match patterns with
      | [] -> raise_str "internal error (type_decl_str)"
      | hd :: [] -> hd, List.hd forcelazy
      | _ -> Pat.tuple patterns, tuple forcelazy
    in
    Vb.mk pattern
      (wrap_runtime (
          createnode @@
          Exp.let_ Recursive lazyrec @@
          setnode @@
          force))
  in
  List.map (fun x -> Str.value Nonrecursive [x]) (prepare :: substitutions)

(* Type declarations in signature. Generates
 * val <type>_t : <type> ttype
 *)
let sig_of_type_decl ~opt ({ ptype_loc = loc ; _} as td) =
  ignore (opt) ;
  let basetyp =
    match td.ptype_kind with
    | Ptype_abstract
    | Ptype_record _
    | Ptype_variant _ -> basetyp_of_type_decl ~loc td
    | _ -> raise_str ~loc "cannot handle this type in signatures yet"
  in
  let typ = typ_of_free_vars ~loc ~basetyp (free_vars_of_type_decl td) in
  Val.mk {txt=(mangle_type_decl td); loc} typ

let type_decl_sig ~options ~path tds =
  let opt = parse_options ~path options in
  List.map (sig_of_type_decl ~opt) tds
  |> List.map Sig.value

(* inline types *)
let core_type ct =
  let opt = parse_options ~path:[] [] in
  core_type ~rec_:[] ~free:[] ~opt ct
  |> wrap_runtime

(* Register the handler for type declarations in signatures and structures *)
let () =
  Ppx_deriving.(register (create deriver ~type_decl_str ~type_decl_sig
                            ~core_type ()))
