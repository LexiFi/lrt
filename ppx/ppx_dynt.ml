open Ppxlib
open Ast_builder.Default

(* Who are we? *)
type ppx = { pp: string; id: string }
let ppx = { pp = "[@@deriving t]" ; id = "t" }

let raise_errorf ~loc =
  Format.ksprintf (Location.raise_errorf ~loc "%s: %s" ppx.pp)

(* Data stored between invocations of the ppx driver *)
module Cookies = struct
  type t = { mutable libname: string option }
  let t = { libname = None }

  let () =
    Driver.Cookies.add_simple_handler "library-name" Ast_pattern.(estring __)
      ~f:(function x -> t.libname <- x)
end

(*
 * generate regularly used AST fragments
 *
 *)

let ignore ~loc expr : expression -> expression =
  let pat = ppat_any ~loc in
  pexp_let ~loc Nonrecursive [value_binding ~loc ~expr ~pat]

let lazy_value_binding ~loc txt basetyp expr =
  let (module M) = Ast_builder.make loc in
  let open M in
  let pat = ppat_constraint (ppat_var {loc;txt})
      [%type: [%t basetyp] Lazy.t] in
  let expr = ignore ~loc (evar txt) [%expr lazy [%e expr]] in
  value_binding ~pat ~expr

let force_lazy ~loc var = [%expr Lazy.force [%e var]]

let stypes_of_free ~loc free =
  let (module M) = Ast_builder.make loc in
  let open M in
  List.mapi (fun i _v -> [%expr DT_var [%e eint i]]) free |> elist

let wrap_runtime ~loc =
  let txt = (Longident.parse "Ppx_dynt_runtime") in
  pexp_open ~loc Override {txt;loc}

let wrap_props ~loc props t =
  match props with
  | [] -> t
  | l -> [%expr
    ttype_of_stype (DT_prop ([%e elist ~loc l] , stype_of_ttype [%e t]))]

(*
 * mangle names
 *
 *)

let mangle_label = function
  | "t" -> ppx.id
  | s -> Format.sprintf "%s_%s" s ppx.id

let mangle_lid = function
  | Lident s -> Lident (mangle_label s)
  | Ldot (t, s) -> Ldot (t, mangle_label s)
  | Lapply _-> raise_errorf ~loc:Location.none "Internal error in mangle_lid"

let mangle_label_loc t = { t with txt = mangle_label t.txt }
let mangle_lid_loc t = { t with txt = mangle_lid t.txt }

(*
 * Read information from AST fragments
 *
 *)

type names = { typ : string; ttyp : string; node : string }

let free_vars_of_type_decl td =
  List.map (fun (ct, _variance) ->
      match ct.ptyp_desc with
      | Ptyp_var s -> s
      | _ -> raise_errorf ~loc:ct.ptyp_loc "This should be a type variable")
    td.ptype_params

let names_of_type_decl td =
  let typ = td.ptype_name.txt in
  { typ ; ttyp = mangle_label typ ; node = typ ^ "_node"}

let type_of_type_decl ~loc td : core_type =
  let (module M) = Ast_builder.make loc in
  let open M in
  ptyp_constr {txt=Lident td.ptype_name.txt; loc}
    (List.map (fun (ct, _variance) -> ct) td.ptype_params)

let ttype_of_type_decl ~loc td : core_type =
  let (module M) = Ast_builder.make loc in
  let open M in
  let ct  = type_of_type_decl ~loc td in
  ptyp_constr {txt=Longident.parse "Dynt.Types.ttype"; loc} [ct]

let close_ttype ~loc ~free ttype =
    List.fold_left (fun acc name ->
        [%type: [%t ptyp_var ~loc name] Dynt.Types.ttype -> [%t acc]])
      ttype (List.rev free)

(*
 * Declare attributes on type declarations, core types,
 * record field labels and variant constructors
 *
 *)

let attr_prop ctx =
  let prop a b = pexp_tuple ~loc:a.loc
      [estring ~loc:a.loc a.txt; estring ~loc:b.loc b.txt]
  in
  Attribute.declare (ppx.id ^ ".prop")
    ctx
    Ast_pattern.(
      (pexp_apply (estring __') ( no_label (estring __') ^:: nil )
       |> map2 ~f:prop
       |> elist) |||
      (pexp_apply (estring __') ( no_label (estring __') ^:: nil )
       |> map2 ~f:prop
       |> map1 ~f:(fun x -> [x])
      )
      |> single_expr_payload
    )
    (fun l -> l)

let attr_ct_prop = attr_prop Attribute.Context.core_type
let attr_rf_prop = attr_prop Attribute.Context.label_declaration
let attr_vc_prop = attr_prop Attribute.Context.constructor_declaration
let attr_td_prop = attr_prop Attribute.Context.type_declaration

let props_of_attr attr x =
  match Attribute.get attr x with
  | None -> []
  | Some l -> l

let props_of_ct = props_of_attr attr_ct_prop
let props_of_rf = props_of_attr attr_rf_prop
let props_of_vc = props_of_attr attr_vc_prop
let props_of_td = props_of_attr attr_td_prop

let attr_td_abstract =
  Attribute.declare (ppx.id ^ ".abstract")
    Attribute.Context.type_declaration
    Ast_pattern.(single_expr_payload (estring __'))
    (fun name -> name)

let abstract_of_td td = Attribute.get attr_td_abstract td

(*
 * general helpers
 *
 *)

let find_index_opt (l : 'a list) (el : 'a) : int option =
  let i = ref 0 in
  let rec f = function
    | [] -> None
    | hd :: _ when hd = el -> Some !i
    | _ :: tl -> incr i ; f tl
  in f l

(*
 * The actual workers
 *
 *)

(* We use this to store information about recursive types. Which identifiers
 * are used recursively? Is the recursion regular?
 * alist mapping recursive identifiers to their type args *)
type rec_ =
  | Nonrec
  | Rec of (label * label list) list

let rec core_type ~rec_ ~free ({ ptyp_loc = loc ; _ } as ct) : expression =
  let rc = core_type ~rec_ ~free in
  let rcs ct = [%expr stype_of_ttype [%e rc ct]] in
  let constr =
    (* type constructors are handled depending on nonrec flag *)
    match rec_ with
    | Nonrec -> fun id args ->
        let id' = mangle_lid_loc id in
        pexp_apply ~loc (pexp_ident ~loc id')
          (List.map (fun x -> Nolabel, rc x) args)
    | Rec l -> fun id args -> begin
        let id' = mangle_lid_loc id in
        (* recursive identifier? *)
        match List.assoc_opt (Longident.name id.txt) l with
        | Some l ->
          (* regular recursion *)
          let is = List.rev_map (fun x -> x.ptyp_desc) args
          and should = List.rev_map (fun a -> Ptyp_var a) l in
          if is = should then
            pexp_ident ~loc id' |> force_lazy ~loc
          else
            raise_errorf ~loc "non-regular type recursion not supported"
        | None ->
          pexp_apply ~loc (pexp_ident ~loc id')
            (List.map (fun x -> Nolabel, rc x) args)
      end
  in
  let t = match ct.ptyp_desc with
    | Ptyp_tuple l ->
      let args = List.map rcs l in
      [%expr ttype_of_stype (DT_tuple [%e elist ~loc args])]
    | Ptyp_constr (id, args) -> constr id args
    | Ptyp_var vname -> begin
        match find_index_opt free vname with
        | None -> raise_errorf ~loc "please provide closed type"
        | Some i -> [%expr ttype_of_stype (DT_var [%e eint ~loc i])]
      end
    | Ptyp_arrow (label, l, r) ->
      let lab =
        match label with
        | Nolabel -> ""
        | Labelled s -> s
        (* TODO: How do you actually represent optional arguments? *)
        | Optional s -> "?" ^ s
      in
      [%expr ttype_of_stype (DT_arrow ([%e estring ~loc lab],
                                       [%e rcs l],
                                       [%e rcs r]))]
    (* TODO: is the closed flag relevant? *)
    | Ptyp_object (l, _closed_flag) ->
      let fields = List.map (function
          (* TODO properties. But where should they be placed? *)
            ({txt; loc}, _attr, ct) ->
            pexp_tuple ~loc [estring ~loc txt; rcs ct]) l
      in
      [%expr ttype_of_stype (DT_object [%e elist ~loc fields])]
    | _ -> raise_errorf ~loc "type not yet supported"
  in
  wrap_props ~loc (props_of_ct ct) t

let fields_of_record_labels ~rec_ ~free l =
  List.map (fun ({pld_loc = loc; _ } as x) ->
      let props = props_of_rf x in
      let t = core_type ~rec_ ~free x.pld_type in
      [%expr
        ([%e estring ~loc x.pld_name.txt], [%e elist ~loc props], stype_of_ttype [%e t])]
    ) l

let record_labels ~loc ~me ~free ~rec_ l =
  let fields = fields_of_record_labels ~free ~rec_ l in
  let createnode =
    let pat = pvar ~loc me.node
    and expr =
      [%expr create_node
          [%e estring ~loc me.typ]
          [%e stypes_of_free ~loc free]]
    in
    pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr]
  and ttype = [%expr ttype_of_stype (DT_node [%e evar ~loc me.node])]
  and setnode =
    let pat = punit ~loc
    and expr =
      [%expr
        set_node_record [%e evar ~loc me.node]
          ([%e elist ~loc fields], Record_regular)]
    in
    pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr]
  in
  createnode, ttype, setnode

let record_labels_inline ~loc ~free ~rec_ ~name i l =
  let fields = fields_of_record_labels ~free ~rec_ l in
  [%expr
    let [%p pvar ~loc "inline_node"] : Dynt.Types.node =
      create_node [%e estring ~loc name] [%e stypes_of_free ~loc free]
    in
    set_node_record [%e evar ~loc "inline_node"]
      ([%e elist ~loc fields], Record_inline [%e eint ~loc i]);
    DT_node [%e evar ~loc "inline_node"]]

let variant_constructors ~loc ~me ~free ~rec_ l =
  let nconst_tag = ref 0 in
  let constructors =
    List.map (fun ({pcd_loc = loc; _ } as x) ->
        let props = props_of_vc x in
        match x.pcd_args with
        | Pcstr_tuple ctl ->
          if ctl <> [] then incr nconst_tag;
          let l = List.map (fun ct ->
              core_type ~rec_ ~free ct
              |> fun e -> [%expr stype_of_ttype [%e e]]
            ) ctl in
          [%expr ([%e estring ~loc x.pcd_name.txt], [%e elist ~loc props],
                  C_tuple [%e elist ~loc l])]
        | Pcstr_record lbl ->
          let name = Format.sprintf "%s.%s" me.typ x.pcd_name.txt in
          let r = record_labels_inline ~rec_ ~free ~loc
              ~name !nconst_tag lbl
          in
          incr nconst_tag;
          [%expr ([%e estring ~loc x.pcd_name.txt],
                  [%e elist ~loc props], C_inline [%e r])]
      ) l
  in
  let createnode =
    let pat = pvar ~loc me.node
    and expr =
      [%expr create_node [%e estring ~loc me.typ]
          [%e stypes_of_free ~loc free]]
    in
    pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr]
  and ttype = [%expr ttype_of_stype (DT_node [%e evar ~loc me.node])]
  and setnode =
    let pat = punit ~loc
    and expr =
      [%expr
        set_node_variant [%e evar ~loc me.node] [%e elist ~loc constructors] ]
    in
    pexp_let ~loc Nonrecursive [ value_binding ~loc ~pat ~expr]
  in
  createnode, ttype, setnode

let substitution_of_free_vars ~loc ~me ttype free =
  let typ = close_ttype ~loc ~free ttype in
  let expr =
    let arr = List.map (fun v ->
        [%expr stype_of_ttype [%e evar ~loc v]]) free
    in
    List.fold_left (fun acc v -> pexp_fun ~loc Nolabel None (pvar ~loc v) acc)
      [%expr ttype_of_stype (
          substitute [%e pexp_array ~loc arr]
            (stype_of_ttype [%e evar ~loc me.ttyp]))]
      (List.rev free)
    |> wrap_runtime ~loc
  and pat = ppat_constraint ~loc (pvar ~loc me.ttyp) typ
  in value_binding ~loc ~expr ~pat

let str_type_decl ~loc ~path (recflag, tds) =
  let _ = path in
  let extend_let new_ was expr = new_ (was expr) in
  let rec_ =
    match recflag with
    | Nonrecursive -> Nonrec
    | Recursive -> Rec (List.map (fun td ->
        td.ptype_name.txt, free_vars_of_type_decl td) tds)
  in
  let parse (pats, cn, lr, sn, fl, subs) ({ ptype_loc = loc ; _} as td) =
    let me = names_of_type_decl td in
    let basetyp = ttype_of_type_decl ~loc td in
    let free = free_vars_of_type_decl td in
    let pats = (ppat_constraint ~loc (pvar ~loc me.ttyp) basetyp) :: pats in
    let cn, ttype, sn =
      match abstract_of_td td with
      | Some {txt=name;loc} ->
        let f = stypes_of_free ~loc free in
        let t = [%expr
          ttype_of_stype(DT_abstract ([%e estring ~loc name],[%e f]))] in
        cn, t, sn
      | None ->
        match td.ptype_kind with
        | Ptype_abstract -> begin match td.ptype_manifest with
            | None -> raise_errorf ~loc "no manifest found"
            | Some ct ->
              let t = core_type ~rec_ ~free ct in
              cn, t, sn
          end
        | Ptype_record l ->
          let c, t, s = record_labels ~me ~free ~loc ~rec_ l in
          extend_let c cn, t, extend_let s sn
        | Ptype_variant l ->
          let c, t, s = variant_constructors ~me ~free ~loc ~rec_ l in
          extend_let c cn, t, extend_let s sn
        | Ptype_open ->
          raise_errorf ~loc "type kind not yet supported"
    in
    let ttype = wrap_props ~loc (props_of_td td) ttype in
    let lr = lazy_value_binding ~loc me.ttyp basetyp ttype :: lr in
    let fl = force_lazy ~loc (evar ~loc me.ttyp) :: fl in
    let subs = if free = [] then subs else
        (substitution_of_free_vars ~loc ~me basetyp free) :: subs in
    pats, cn, lr, sn, fl, subs
  in
  let patterns, createnode, lazyrec, setnode, forcelazy, substitutions =
    let id = fun x -> x in
    List.fold_left parse ([], id ,[], id,[],[]) tds in
  let prepare =
    let pat, force =
      match patterns with
      | [] -> raise_errorf ~loc "internal error (type_decl_str)"
      | hd :: [] -> hd, List.hd forcelazy
      | _ -> ppat_tuple ~loc patterns, pexp_tuple ~loc forcelazy
    in
    let expr =
      let recflag = match rec_ with Nonrec -> Nonrecursive | _ -> Recursive in
      wrap_runtime ~loc (
        createnode @@
        pexp_let ~loc recflag lazyrec @@
        setnode @@
        force)
    in
    value_binding ~loc ~pat ~expr
  in
  List.map (fun x -> pstr_value ~loc Nonrecursive [x])
    (prepare :: substitutions)

(* Type declarations in signature. Generates
 * val <type>_t : <type> ttype
 *)
let sig_of_type_decl ({ ptype_loc = loc ; _} as td) =
  match td.ptype_kind with
  | Ptype_abstract
  | Ptype_record _
  | Ptype_variant _ ->
    let type_ =
      let free = free_vars_of_type_decl td in
      close_ttype ~loc ~free (ttype_of_type_decl ~loc td)
    and name = mangle_label_loc td.ptype_name
    and prim = []
    in value_description ~loc ~type_~name ~prim
  | _ -> raise_errorf ~loc "cannot handle this type in signatures yet"

let sig_type_decl ~loc ~path (_recflag, tds) =
  let _ = path in
  List.map sig_of_type_decl tds
  |> List.map (psig_value ~loc)

(* inline types *)
let extension ~loc ~path ct =
  let _ = path in
  let t = core_type ~rec_:Nonrec ~free:[] ct in
  (* prepend ignore statement to produce nicer error message *)
  wrap_runtime ~loc [%expr let _ = fun (_ : [%t ct])  -> () in [%e t]]

(* Register the generator functions *)
let () =
  let open Deriving in
  let str_type_decl = Generator.make_noarg str_type_decl in
  let sig_type_decl = Generator.make_noarg sig_type_decl in
  Deriving.add ~str_type_decl ~sig_type_decl ~extension
    ppx.id |> Deriving.ignore