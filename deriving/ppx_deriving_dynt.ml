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

(* Construct ttype generator from core type *)
let rec ttype_of_core_type ~opt ~rec_ ~free ({ ptyp_loc = loc ; _ } as ct) =
  let fail () = raise_str ~loc "type not yet supported" in
  let rc = ttype_of_core_type ~opt ~rec_ ~free in
  let t = match ct.ptyp_desc with
    | Ptyp_tuple l ->
      let args =
        List.fold_left (fun acc e ->
            [%expr stype_of_ttype [%e rc e] :: [%e acc]]) [%expr []]
          (List.rev l)
      in
      [%expr ttype_of_stype (DT_tuple [%e args])]
    | Ptyp_constr (id, args) -> begin
        let id' = { id with txt = mangle_lid id.txt} in
        (* recursive identifier? regular recursion? *)
        match List.assoc_opt (Longident.last id.txt) rec_ with
        | Some l ->
          let is = List.rev_map (fun x -> x.ptyp_desc) args
          and should = List.map (fun a -> Ptyp_var a) l in
          if is = should then
            Exp.ident id' |> force_lazy ~loc
          else
            raise_str ~loc "non-regular type recursion not supported"
        | None ->
          List.fold_left
            (fun acc e -> [%expr [%e acc] [%e rc e]])
            (Exp.ident id') args
      end
    | Ptyp_var vname -> begin
        match find_index_opt free vname with
        | None -> assert false
        | Some i -> [%expr ttype_of_stype (DT_var [%e int i])]
      end
    | _ -> fail ()
  in
  match opt.abstract with
  | Some name ->
    [%expr ttype_of_stype( DT_abstract ([%e str name],[]))]
  | None -> t

let stypes_of_free ~loc free =
  List.mapi (fun i _v -> [%expr DT_var [%e int i]]) free |> list

(* Construct record ttypes *)
let record_fields_of_record_labels ~opt ~rec_ ~free l =
  List.map (fun {pld_loc = loc; pld_name; pld_type; _ } ->
      let t = ttype_of_core_type ~opt ~rec_ ~free pld_type in
      [%expr
        ([%e str pld_name.txt], [], stype_of_ttype [%e t])]
    ) l

let single_let_in pat expr =
  let_in [Vb.mk pat expr]

let record_labels ~loc ~opt ~me ~free ~rec_ l =
  let fields = record_fields_of_record_labels ~opt ~free ~rec_ l in
  let createnode = single_let_in (pvar me.node)
      [%expr create_node [%e str me.typ] [%e stypes_of_free ~loc free]]
  and ttype = [%expr ttype_of_stype (DT_node [%e evar me.node])]
  and setnode = single_let_in (punit ())
      [%expr
        set_node_record [%e evar me.node] ([%e list fields], Record_regular) ]
  in
  createnode, ttype, setnode

let inline_record_stype_of_record_labels ~loc ~opt ~free ~rec_ ~name i l =
  let fields = record_fields_of_record_labels ~opt ~free ~rec_ l in
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
    List.map (fun {pcd_loc = loc; pcd_name; pcd_args; _ } ->
      match pcd_args with
      | Pcstr_tuple ctl ->
        if ctl <> [] then incr nconst_tag;
        let l = List.rev_map (fun ct ->
            ttype_of_core_type ~opt ~rec_ ~free ct
            |> fun e -> [%expr stype_of_ttype [%e e]]
          ) ctl in
        [%expr ([%e str pcd_name.txt], [],
                C_tuple [%e expr_list ~loc l])]
      | Pcstr_record lbl ->
        let name = sprintf "%s.%s" me.typ pcd_name.txt in
        let r = inline_record_stype_of_record_labels ~rec_ ~free ~opt ~loc
            ~name !nconst_tag lbl
        in
        incr nconst_tag;
        [%expr ([%e str pcd_name.txt], [], C_inline [%e r])]
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
  List.rev_map (fun (ct, _variance) ->
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
    basetyp free

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
      free
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
            let t = ttype_of_core_type ~rec_ ~opt ~free ct in
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
      | [] -> assert false
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

(* Register the handler for type declarations in signatures and structures *)
let () =
  Ppx_deriving.(register (create deriver ~type_decl_str ~type_decl_sig ()))
