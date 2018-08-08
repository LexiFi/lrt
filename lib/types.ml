module TypEq : sig

  type (_, _) t = Eq: ('a, 'a) t

  val refl: ('a, 'a) t
  val trans: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val sym: ('a, 'b) t -> ('b, 'a) t

  val app: ('a, 'b) t -> 'a -> 'b

  module Lift(T : sig type 'a c end) : sig
    val eq: ('a, 'b) t -> ('a T.c, 'b T.c) t
  end

  val unsafe: ('a, 'b) t

end = struct

  type (_, _) t = Eq: ('a, 'a) t

  let refl = Eq

  let trans (type a) (type b) (type c) (Eq : (a, b) t) (Eq : (b, c) t) =
    (Eq : (a, c) t)

  let sym (type a) (type b) (Eq : (a, b) t) = (Eq : (b, a) t)

  let app (type a) (type b) (Eq : (a, b) t) (x : a) = (x : b)

  let unsafe = Obj.magic Eq

  module Lift(T : sig type 'a c end) = struct
    let eq (type a) (type b) (Eq : (a, b) t) = (Eq : (a T.c, b T.c) t)
  end

end

type stype_properties = (string * string) list

type record_repr = Record_regular | Record_float | Record_inline of int

type 'node gtype =
  | DT_node of 'node
  | DT_int
  | DT_float
  | DT_string
  | DT_date
  | DT_tuple of 'node gtype list
  | DT_list of 'node gtype
  | DT_array of 'node gtype
  | DT_option of 'node gtype
  | DT_abstract of string * 'node gtype list
  | DT_arrow of string * 'node gtype * 'node gtype
  | DT_object of (string * 'node gtype) list
  | DT_prop of stype_properties * 'node gtype
  | DT_var of int

let unnode = function
  | DT_node n -> n
  | _ -> assert false

let nonrec_map fnode f = function
  | DT_node n -> DT_node (fnode n)
  | DT_int -> DT_int
  | DT_float -> DT_float
  | DT_string -> DT_string
  | DT_date -> DT_date
  | DT_tuple tl -> DT_tuple (List.map f tl)
  | DT_list t -> DT_list (f t)
  | DT_array t -> DT_array (f t)
  | DT_option t -> DT_option (f t)
  | DT_abstract (s, tl) -> DT_abstract (s, List.map f tl)
  | DT_arrow (l, t1, t2) -> DT_arrow (l, f t1, f t2)
  | DT_object fl -> DT_object (List.map (fun (s, t) -> (s, f t)) fl)
  | DT_prop (p, t) -> DT_prop (p, f t)
  | DT_var i -> DT_var i

let gmap ~ignore_props f =
  if ignore_props then
    let rec aux = function
      | DT_prop (_, t) -> aux t
      | t -> nonrec_map node aux t
    and node n = f aux n
    in
    aux
  else
    let rec aux t = nonrec_map (f aux) aux t in
    aux

let equal_list f l1 l2 =
  List.length l1 = List.length l2 && List.for_all2 f l1 l2

type memoized_type_prop = ..

type stype = node gtype

and node = {
  mutable rec_descr: node_descr;
  rec_uid: int;
  rec_name: string;
  rec_args: stype list;
  mutable rec_has_var: bool option;
  mutable rec_hash: int;
  mutable rec_memoized: memoized_type_prop array;
}

and node_descr =
  | DT_variant of variant_descr
  | DT_record of record_descr

and variant_descr = {
  variant_constrs: (string * stype_properties * stype variant_args) list;
}
and 'stype variant_args =
  | C_tuple of 'stype list
  | C_inline of 'stype
  (* [@@mlfi.dyn {of_variant_inject_on_type_error="C_tuple INJECTION"}] *)


and record_descr = {
  record_fields: (string * stype_properties * stype) list;
  record_repr: record_repr;
}

type list_sep =
  | STAR
  | COMMA


let is_enumeration = function
  | DT_node{rec_name = "Mlfi_isdatypes.variant"; _} -> true
  | DT_node{rec_descr = DT_variant {variant_constrs}; _} ->
    List.for_all (fun (_, _, args) -> args = C_tuple []) variant_constrs
  | _ -> false

let uninline = function
  | C_tuple tl -> tl
  | C_inline t -> [t]

let is_cst_args = function
  | C_tuple [] -> true
  | _ -> false

let print_stype ~show_enumerations ppf =
  let seen = Hashtbl.create 8 in
  let rec is_complex t =
    (show_enumerations || not (is_enumeration t)) &&
    match t with
    | DT_node n -> not (Hashtbl.mem seen n.rec_uid)
    | DT_list t
    | DT_array t
    | DT_option t
    | DT_prop (_, t) -> is_complex t
    | DT_tuple l -> List.exists is_complex l
    | _ -> false
  in
  let fprintf = Format.fprintf in
  let rec aux ppf t =
    match t with
    | DT_node n when Hashtbl.mem seen n.rec_uid ||
                     (not show_enumerations && is_enumeration t) -> name ppf n
    | DT_node n ->
      Hashtbl.replace seen n.rec_uid ();
      fprintf ppf "(@[<v2>%a =%a@])" name n descr n.rec_descr
    | DT_int -> fprintf ppf "int"
    | DT_float -> fprintf ppf "float"
    | DT_string -> fprintf ppf "string"
    | DT_date -> fprintf ppf "date"
    | DT_tuple tl -> tlist STAR ppf tl
    | DT_list t -> fprintf ppf "%a list" aux t
    | DT_array t -> fprintf ppf "%a array" aux t
    | DT_option t -> fprintf ppf "%a option" aux t
    | DT_abstract (s, []) -> fprintf ppf "%s" s
    | DT_abstract (s, tl) -> fprintf ppf "%a %s" (tlist COMMA) tl s
    | DT_arrow ("", t1, t2) -> fprintf ppf "(%a -> %a)" aux t1 aux t2
    | DT_arrow (l, t1, t2) -> fprintf ppf "(%s:%a -> %a)" l aux t1 aux t2
    | DT_object fl ->
      fprintf ppf "@ <@[<v1>";
      List.iter
        (fun (s, t) ->
           fprintf ppf "@ %s" s;
           fprintf ppf ": ";
           if is_complex t then
             fprintf ppf "@ ";
           aux ppf t;
           fprintf ppf ";"
        )
        fl;
      fprintf ppf "@]@ >"

    | DT_prop (p, t) -> fprintf ppf "%a%a" aux t props p;
    | DT_var i -> fprintf ppf "$%i" i
  and props ppf p =
    fprintf ppf " + [";
    let first = ref true in
    List.iter
      (fun (k,v) ->
         if !first then first := false else fprintf ppf "; ";
         if v = "" then fprintf ppf "%s" k
         else fprintf ppf "%s = %S" k v
      )
      p;
    fprintf ppf "]"
  and tlist sep ppf = function
    | [] -> fprintf ppf "[]"
    | [x] -> aux ppf x
    | hd::tl as l ->
      fprintf ppf "(";
      if List.exists is_complex l then
        fprintf ppf "@[<v>"
      else
        fprintf ppf "@[<h>";
      aux ppf hd;
      List.iter
        (fun x ->
           begin
             match sep with
             | STAR -> fprintf ppf "@ *@ ";
             | COMMA -> fprintf ppf ",@ "
           end;
           aux ppf x
        ) tl;
      fprintf ppf "@])"
  and name ppf n =
    match n.rec_args with
    | [] -> fprintf ppf "%s" n.rec_name
    | l -> fprintf ppf "%a %s" (tlist COMMA) l n.rec_name
  and descr ppf = function
    | DT_variant v ->
      let simple =
        List.for_all (fun (_, _, args) -> args = C_tuple []) v.variant_constrs
      in
      if simple then
        fprintf ppf "@[<h>";
      let first = ref simple in
      List.iter
        (fun (c, p, args) ->
           if not !first then
             fprintf ppf "@ | "
           else
             begin
               fprintf ppf "@ ";
               first := false;
             end;
           fprintf ppf "%s" c;
           if p <> [] then props ppf p;
           let args = uninline args in
           if args <> [] then
             begin
               fprintf ppf " of";
               if List.exists is_complex args then
                 fprintf ppf "@ ";
               fprintf ppf " %a" (tlist STAR) args;
             end
        )
        v.variant_constrs;
      if simple then
        fprintf ppf "@]";
    | DT_record r ->
      fprintf ppf "@ {@[<v1>";
      List.iter
        (fun (c, p, arg) ->
           fprintf ppf "@ %s" c;
           if p <> [] then props ppf p;
           fprintf ppf ": ";
           if is_complex arg then
             fprintf ppf "@ ";
           aux ppf arg;
           fprintf ppf ";"
        )
        r.record_fields;
      fprintf ppf "@]@ }"
  in
  aux ppf

let print_stype_hide_enumerations = print_stype ~show_enumerations: false

let print_stype_ref = ref (print_stype ~show_enumerations: true)

let print_stype ppf =
  !print_stype_ref ppf

module Internal = struct

  let dummy_descr = DT_variant {variant_constrs=[]}
  let uid = ref 0

  let create_node name args =
    incr uid;
    {rec_descr=dummy_descr; rec_uid = !uid; rec_name=name; rec_args=args;
     rec_has_var=None; rec_hash=0; rec_memoized=[||]; }

  let set_node_variant n constrs =
    n.rec_descr <- DT_variant {variant_constrs=constrs}
  let set_node_record n (fields, repr) =
    n.rec_descr <- DT_record {record_fields=fields; record_repr=repr}

  let map_node ~ignore_props aux memo n =
    let prop = if ignore_props then fun _p -> [] else fun p -> p in
    let n' = create_node n.rec_name (List.map aux n.rec_args) in
    memo n';
    n'.rec_descr <- begin
      match n.rec_descr with
      | DT_variant v ->
        let variant_constrs = List.map (
            fun (s, p, args) ->
              let args =  match args with
                | C_tuple tl -> C_tuple (List.map aux tl)
                | C_inline t -> C_inline (aux t)
              in
              (s, prop p, args)
          ) v.variant_constrs in
        DT_variant { variant_constrs }
      | DT_record r ->
        let record_fields = List.map (fun (s, p, t) ->
            (s, prop p, aux t)) r.record_fields
        in DT_record {r with record_fields }
    end;
    n'

  let create_variant_type name args f =
    let n = create_node name args in
    let t = DT_node n in
    set_node_variant n (f t);
    t

  let create_record_type name args f =
    let n = create_node name args in
    let t = DT_node n in
    set_node_record n (f t);
    t

  let name ~ignore_path n =
    let s = n.rec_name in
    if ignore_path then
      try
        let i = String.rindex s '.' in
        String.sub s (i + 1) (String.length s - i - 1)
      with Not_found -> s
    else s

  type sstype = Rec of string * sstype gtype list

  let rec hash0_node _ n =
    if n.rec_hash <> 0 then n.rec_hash
    else
      let h = (name ~ignore_path:true n, List.map hash0 n.rec_args,
               String.concat ","
                 (match n.rec_descr with
                  | DT_variant v ->
                    List.map (fun (s, _, _) -> s) v.variant_constrs
                  | DT_record r ->
                    List.map (fun (s, _, _) -> s) r.record_fields)
              ) |> Hashtbl.hash
      in
      n.rec_hash <- h;
      h

  and hash0 t = Hashtbl.hash (gmap ~ignore_props:true hash0_node t)

  let hash0_node = hash0_node (fun _ -> assert false)

  let hash ~ignore_props ~ignore_path t =
    let f aux n = Rec (name ~ignore_path n, List.map aux n.rec_args) in
    Hashtbl.hash (gmap ~ignore_props f t)

  let equal ~ignore_props ~ignore_path =
    let memo = Hashtbl.create 16 in
    let stack = ref [] in

    let rec eq n1 n2 =
      let id1 = n1.rec_uid and id2 = n2.rec_uid in
      id1 = id2 ||
      (hash0_node n1 = hash0_node n2) &&
      let k = if id1 < id2 then (id1, id2) else (id2, id1) in
      try Hashtbl.find memo k
      with Not_found ->
        (name ~ignore_path n1 = name ~ignore_path n2)
        && equal_list geq n1.rec_args n2.rec_args
        && begin
          (* Coinductive algorithm: first assume that the two types
             are equivalent. If we discover some difference, remember that
             they are different. At the end, we remove all the equalities
             that might depend on a false initial assumption. *)

          Hashtbl.replace memo k true;
          stack := k :: !stack;
          begin match n1.rec_descr, n2.rec_descr with
            | DT_variant v1, DT_variant v2 ->
              let var_args_eq args1 args2 =
                match args1, args2 with
                | C_tuple tl1, C_tuple tl2 -> equal_list geq tl1 tl2
                | C_inline t1, C_inline t2 -> geq t1 t2
                | _ -> false
              in
              let constr (c1, p1, tl1) (c2, p2, tl2) =
                c1 = c2 && (ignore_props || p1 = p2) && (var_args_eq tl1 tl2)
              in
              equal_list constr v1.variant_constrs v2.variant_constrs
            | DT_record r1, DT_record r2 ->
              let field (f1, p1, t1) (f2, p2, t2) =
                f1 = f2 && (ignore_props || p1 = p2) && geq t1 t2
              in
              equal_list field r1.record_fields r2.record_fields
            | _ -> false
          end || (Hashtbl.replace memo k false; false)
        end

    and geq t1 t2 =
      t1 == t2 ||
      match t1, t2 with
      | DT_prop (_, t1), t2 | t1, DT_prop (_, t2) when ignore_props ->
        geq t1 t2
      | DT_node n1, DT_node n2 -> eq n1 n2
      | DT_tuple tl1, DT_tuple tl2 -> equal_list geq tl1 tl2
      | DT_list t1, DT_list t2
      | DT_array t1, DT_array t2
      | DT_option t1, DT_option t2 -> geq t1 t2
      | DT_abstract (t1, tl1), DT_abstract (t2, tl2) ->
        t1 = t2 && equal_list geq tl1 tl2
      | DT_arrow (l1, t1, s1), DT_arrow (l2, t2, s2) ->
        l1 = l2 && geq t1 t2 && geq s1 s2
      | DT_object fl1, DT_object fl2 ->
        equal_list (fun (s1, t1) (s2, t2) -> s1 = s2 && geq t1 t2) fl1 fl2
      | DT_prop (p1, t1), DT_prop (p2, t2) -> p1 = p2 && geq t1 t2
      | DT_var i1, DT_var i2 -> i1 = i2
      | _ -> false
    in
    fun t1 t2 ->
      let r = geq t1 t2 in
      if not r && !stack != [] then begin
        List.iter (function k ->
            if Hashtbl.find memo k then Hashtbl.remove memo k) !stack;
        stack := [];
      end;
      r

  (* Normalization functions based on some equality *)

  let normalizer eq =
    let module H = Hashtbl.Make(struct
        type t = node
        let equal n1 n2 = eq (DT_node n1) (DT_node n2)
        let hash = hash0_node
      end) in
    let memo = H.create 128 in
    fun t ->
      try H.find memo t
      with Not_found ->
        H.replace memo t t;
        t

  let normalizer_node ~ignore_props ~ignore_path =
    normalizer (equal ~ignore_props ~ignore_path)

  (* Check if a stype has any substituable variable. *)

  let has_var t =
    let varstack = ref [] in

    let rec node = function
      | {rec_has_var = Some b; _} -> b
      | n ->
        if List.exists aux n.rec_args then (n.rec_has_var <- Some true; true)
        else begin
          n.rec_has_var <- Some false;
          varstack := n :: !varstack;
          begin match n.rec_descr with
            | DT_variant v ->
              List.exists (fun (_, _, tl) ->
                  List.exists aux (uninline tl)) v.variant_constrs
            | DT_record r ->
              List.exists (fun (_, _, t) -> aux t) r.record_fields
          end && (n.rec_has_var <- Some true; true)
        end
    and aux = function
      | DT_var _ -> true
      | DT_int | DT_float | DT_string | DT_date -> false
      | DT_prop (_, t) | DT_list t | DT_array t | DT_option t -> aux t
      | DT_abstract (_, tl) | DT_tuple tl -> List.exists aux tl
      | DT_arrow (_, t1, t2) -> aux t1 || aux t2
      | DT_object fl -> List.exists (fun (_, t) -> aux t) fl
      | DT_node n -> node n
    in
    let r = aux t in
    if r then List.iter (function
        | { rec_has_var = Some false; _} as n -> n.rec_has_var <- None
        | _ -> ()) !varstack;
    r

  let node_has_var = function
    | {rec_has_var = Some b; _} -> b
    | n -> has_var (DT_node n)

  (* Substitution *)

  let debug =
    try Sys.getenv "DEBUG_MLFI_TYPES" <> ""
    with Not_found -> false

  let substitute subst t =
    if debug then begin
      Format.printf "  SUBST t=%a@." print_stype t;
      Array.iteri (fun i _t ->
          Format.printf " SUBST %i -> %a@." i print_stype subst.(i)) subst;
    end;
    let tbl = Hashtbl.create 4 in
    let rec aux = function
      | DT_var i -> subst.(i)
      | t -> nonrec_map node aux t
    and node n =
      if not (node_has_var n) then n
      else
        try Hashtbl.find tbl n.rec_uid
        with Not_found ->
          if debug then Format.printf " SUBST #%i@." n.rec_uid;
          map_node ~ignore_props:false aux (Hashtbl.replace tbl n.rec_uid) n
    in
    aux t

  (* Normalization *)

  let mapper ~ignore_props f =
    let tbl = Hashtbl.create 4 in
    let rec node aux n =
      try Hashtbl.find tbl n.rec_uid
      with Not_found ->
        let n1 = f n in
        if n != n1 then
          (let n2 = node aux n1 in Hashtbl.replace tbl n.rec_uid n2; n2)
        else map_node ~ignore_props aux (Hashtbl.replace tbl n.rec_uid) n
    in
    gmap ~ignore_props node

  let normalize ~ignore_props ~ignore_path =
    mapper ~ignore_props (normalizer_node ~ignore_props ~ignore_path)

  let remove_props =
    mapper ~ignore_props:true (fun n -> n)


  let set_memoized n a = n.rec_memoized <- a
end

(* Textual form for import-export *)

module Textual = struct
  type t = int gtype

  type node =
    | Variant of
        string * t list * (string * stype_properties * t variant_args) list
    | Record of
        string * t list * (string * stype_properties * t) list * record_repr

  type textual = {
    nodes: node array;
    t: t;
  }

  module StringShare = Weak.Make(struct
      type t = string
      let hash = Hashtbl.hash
      let equal = (=)
    end)

  let string_share = StringShare.create 512

  let share s = StringShare.merge string_share s

  let share_props l =
    List.map (fun (k, v) -> share k, share v) l

  let export_ t =
    let txt_nodes = Hashtbl.create 8 and counter = ref 0 in
    let nodes = Hashtbl.create 8 in
    let tbl = Hashtbl.create 4 in
    let node aux n : int =
      try Hashtbl.find tbl n.rec_uid
      with Not_found ->
        let args = List.map aux n.rec_args in
        let name = n.rec_name in
        let id = !counter in
        incr counter;
        Hashtbl.replace tbl n.rec_uid id;
        let d = match n.rec_descr with
          | DT_variant v ->
            let constrs =
              List.map (fun (s, p, args) ->
                  (share s, share_props p,
                   match args with
                   | C_tuple tl -> C_tuple (List.map aux tl)
                   | C_inline t -> C_inline (aux t)
                  )
                ) v.variant_constrs
            in
            Variant (name, args, constrs)
          | DT_record r ->
            let fields =
              List.map (fun (s, p, t) ->
                  (share s, share_props p, aux t)) r.record_fields
            in
            Record (name, args, fields, r.record_repr)
        in
        Hashtbl.add nodes id n;
        Hashtbl.add txt_nodes id d;
        id
    in
    let t = gmap ~ignore_props:false node t in
    {
      nodes = Array.init !counter (Hashtbl.find txt_nodes);
      t;
    },
    Array.init !counter (Hashtbl.find nodes)

  let export t = fst (export_ t)

  let export_with_digests t =
    let (txt, nodes) = export_ t in
    let digest n =
      let tbl = export (DT_node n) in
      Digest.string (Marshal.to_string tbl [Marshal.No_sharing])
    in
    let digests = Array.map digest nodes in
    assert(Array.length digests = Array.length txt.nodes);
    txt, digests


  let digest2node = Hashtbl.create 256

  let import d digests =
    let nodes = Array.make (Array.length d.nodes) None in
    let really_node aux i =
      match d.nodes.(i) with
      | Variant (name, args, constrs) ->
        unnode (Internal.create_variant_type name (List.map aux args)
                  (fun node ->
                     nodes.(i) <- Some (unnode node);
                     List.map (fun (s, p, args) ->
                         (share s, share_props p,
                          match args with
                          | C_tuple tl -> C_tuple (List.map aux tl)
                          | C_inline t -> C_inline (aux t)
                         )
                       ) constrs
                  ))
      | Record (name, args, fields, repr) ->
        unnode (Internal.create_record_type name (List.map aux args)
                  (fun node ->
                     nodes.(i) <- Some (unnode node);
                     (List.map (fun (s, p, t) ->
                          (share s, share_props p, aux t)) fields, repr)
                  ))
    in
    let node aux i =
      match nodes.(i) with
      | Some v -> v
      | None ->
        if i < Array.length digests then
          let digest = digests.(i) in
          try
            let r = Hashtbl.find digest2node digest in
            nodes.(i) <- Some r;
            r
          with Not_found ->
            let r = really_node aux i in
            Hashtbl.replace digest2node digest r;
            r
        else
          really_node aux i
    in
    gmap ~ignore_props:false node d.t

  let import_table txt digests =
    assert(Array.length txt.nodes = Array.length digests);
    match import txt digests with
    | DT_tuple l -> Array.of_list l
    | _ -> assert false

  let import t = import t [||]
end

(************************ statisfy interface **********************************)

type 'a ttype = stype

external stype_of_ttype: _ ttype -> stype = "%identity"

external internal_ttype_of: 'a -> 'a ttype = "%typeof"

external __use_ttype: 'a ttype -> 'b -> 'b = "%use_ttype"

let rec remove_first_props = function
  | DT_prop(_, t) -> remove_first_props t
  | x -> x

let remove_first_props_ttype = remove_first_props
let add_props props t = DT_prop(props, t)

let rec abstract_ttype = function
  | DT_node {rec_name=name; rec_args=l; _} -> DT_abstract(name, l)
  | DT_prop(props, t) -> DT_prop(props, abstract_ttype t)
  | _ ->
    failwith "abstract_ttype only applies to variant or record types."

let abstract_stype = abstract_ttype

let split_arrow_ttype t =
  match remove_first_props t with
  | DT_arrow(_, t1, t2) -> t1, t2
  | _ -> assert false

let strict_types_equality =
  Internal.equal ~ignore_props:false ~ignore_path:false
let types_equality_modulo_props =
  Internal.equal ~ignore_props:true ~ignore_path:true
let types_equality = 
  Internal.equal ~ignore_props:false ~ignore_path:true

let strict_types_equality t1 t2 =
  match t1, t2 with
  | DT_node n1, DT_node n2 ->
    n1 == n2 || (n1.rec_name = n2.rec_name && strict_types_equality t1 t2)
  | DT_node _, _
  | _, DT_node _ -> false
  | _ -> strict_types_equality t1 t2

let ttypes_equality t1 t2 =
  if types_equality t1 t2 then Some TypEq.unsafe
  else None

let ttypes_equality_modulo_props t1 t2 =
  if types_equality_modulo_props t1 t2 then Some TypEq.unsafe
  else None

let build_arrow_ttype t1 t2 =
  DT_arrow("", t1, t2)

let ttype_fst = function DT_tuple [t; _] -> t | _ -> assert false
let ttype_snd = function DT_tuple [_; t] -> t | _ -> assert false

module StackTrace = struct
  type expr =
    | Longident of string
    | Apply of expr * (string * expr) list
    | Construct of string * expr option
    | Tuple of expr list
    | Int of int
    | String of string
    | Float of string
    | Unknown

  type call_site = {
    directory: string;
    filename: string;
    line: int;
    expr: expr;
  }

  let rec print_expr ppf = function
    | Longident lid -> Format.fprintf ppf "%s" lid
    | Apply (Longident "^", ["", e1; "", e2]) ->
      Format.fprintf ppf "(%a ^ %a)" print_expr e1 print_list e2
    | Apply (f, args) ->
      Format.fprintf ppf "(%a%a)" print_expr f print_args args
    | Construct ("::", Some (Tuple [_;_])) as e ->
      Format.fprintf ppf "(%a)" print_list e
    | Construct (c, None) ->
      Format.fprintf ppf "%s" c
    | Construct (c, Some e) ->
      Format.fprintf ppf "(%s %a)" c print_expr e
    | Tuple el ->
      Format.fprintf ppf "(%a)" print_tuple el
    | Int i ->
      Format.fprintf ppf "%i" i
    | String s ->
      Format.fprintf ppf "%S" s
    | Float s ->
      Format.fprintf ppf "%s" s
    | Unknown ->
      Format.fprintf ppf "<expr>"
  and print_list ppf = function
    | Construct ("::", Some (Tuple [e1;e2])) ->
      Format.fprintf ppf "%a :: %a" print_expr e1 print_list e2
    | e -> print_expr ppf e
  and print_args ppf = function
    | [] -> ()
    | ("", e) :: rest ->
      Format.fprintf ppf " %a" print_expr e; print_args ppf rest
    | (l, e) :: rest when l.[0] = '?' ->
      Format.fprintf ppf " %s:%a" l print_expr e; print_args ppf rest
    | (l, e) :: rest ->
      Format.fprintf ppf " ~%s:%a" l print_expr e; print_args ppf rest
  and print_tuple ppf = function
    | [] -> ()
    | [hd] ->
      Format.fprintf ppf "%a" print_expr hd
    | hd::tl ->
      Format.fprintf ppf "%a, " print_expr hd; print_tuple ppf tl

  let print_call_site ppf cs =
    Format.fprintf ppf "%s:%i:%a" cs.filename cs.line print_expr cs.expr
end
