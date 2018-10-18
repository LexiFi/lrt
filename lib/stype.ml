(* TODO: pk: It feels like the unboxed/boxed flag should be stored at the level
 * of variants and records. Now it is put into the (single) field / constructor.
 * This lead to indirections, when accessing the flag. *)

type properties = (string * string) list

type record_repr =
  | Record_regular
  | Record_float
  | Record_unboxed
  | Record_inline of int

type variant_repr = Variant_regular | Variant_unboxed

type 'node gtype =
  | DT_node of 'node
  | DT_int
  | DT_float
  | DT_string
  (* | DT_date *)
  | DT_tuple of 'node gtype list
  | DT_list of 'node gtype
  | DT_array of 'node gtype
  | DT_option of 'node gtype
  | DT_abstract of string * 'node gtype list
  | DT_arrow of string * 'node gtype * 'node gtype
  | DT_object of (string * 'node gtype) list
  | DT_prop of properties * 'node gtype
  | DT_var of int

let unnode = function DT_node n -> n | _ -> assert false

let nonrec_map fnode f = function
  | DT_node n -> DT_node (fnode n)
  | DT_int -> DT_int
  | DT_float -> DT_float
  | DT_string -> DT_string
  (* | DT_date -> DT_date *)
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
    and node n = f aux n in
    aux
  else
    let rec aux t = nonrec_map (f aux) aux t in
    aux

let equal_list f l1 l2 =
  List.length l1 = List.length l2 && List.for_all2 f l1 l2

type memoized_type_prop = ..

type t = node gtype

and node =
  { mutable rec_descr: node_descr
  ; rec_uid: int
  ; rec_name: string
  ; rec_args: t list
  ; mutable rec_has_var: bool option
  ; mutable rec_hash: int
  ; mutable rec_memoized: memoized_type_prop array }

and node_descr = DT_variant of variant_descr | DT_record of record_descr

and variant_descr =
  { variant_constrs: (string * properties * t variant_args) list
  ; variant_repr: variant_repr }

and 'stype variant_args = C_tuple of 'stype list | C_inline of 'stype

(* [@@mlfi.dyn {of_variant_inject_on_type_error="C_tuple INJECTION"}] *)
and record_descr =
  {record_fields: (string * properties * t) list; record_repr: record_repr}

type list_sep = STAR | COMMA

let is_enumeration = function
  (* | DT_node{rec_name = "Mlfi_isdatypes.variant"; _} -> true *)
  | DT_node {rec_descr= DT_variant {variant_constrs; _}; _} ->
      List.for_all (fun (_, _, args) -> args = C_tuple []) variant_constrs
  | _ -> false

let uninline = function C_tuple tl -> tl | C_inline t -> [t]
let is_cst_args = function C_tuple [] -> true | _ -> false

let rec remove_outer_props = function
  | DT_prop (_, t) -> remove_outer_props t
  | x -> x

let rec consume_outer_props acc = function
  | DT_prop (props, t) -> consume_outer_props (props @ acc) t
  | x -> (acc, x)

let consume_outer_props = consume_outer_props []

let print_stype ~show_enumerations ppf =
  let seen = Hashtbl.create 8 in
  let rec is_complex t =
    (show_enumerations || not (is_enumeration t))
    &&
    match t with
    | DT_node n -> not (Hashtbl.mem seen n.rec_uid)
    | DT_list t | DT_array t | DT_option t | DT_prop (_, t) -> is_complex t
    | DT_tuple l -> List.exists is_complex l
    | _ -> false
  in
  let fprintf = Format.fprintf in
  let rec aux ppf t =
    match t with
    | DT_node n
      when Hashtbl.mem seen n.rec_uid
           || ((not show_enumerations) && is_enumeration t) ->
        name ppf n
    | DT_node n ->
        Hashtbl.replace seen n.rec_uid () ;
        fprintf ppf "(@[<v2>%a =%a@])" name n descr n.rec_descr
    | DT_int -> fprintf ppf "int"
    | DT_float -> fprintf ppf "float"
    | DT_string -> fprintf ppf "string"
    (* | DT_date -> fprintf ppf "date" *)
    | DT_tuple tl -> tlist STAR ppf tl
    | DT_list t -> fprintf ppf "%a list" aux t
    | DT_array t -> fprintf ppf "%a array" aux t
    | DT_option t -> fprintf ppf "%a option" aux t
    | DT_abstract (s, []) -> fprintf ppf "%s" s
    | DT_abstract (s, tl) -> fprintf ppf "%a %s" (tlist COMMA) tl s
    | DT_arrow ("", t1, t2) -> fprintf ppf "(%a -> %a)" aux t1 aux t2
    | DT_arrow (l, t1, t2) -> fprintf ppf "(%s:%a -> %a)" l aux t1 aux t2
    | DT_object fl ->
        fprintf ppf "@ <@[<v1>" ;
        List.iter
          (fun (s, t) ->
            fprintf ppf "@ %s" s ;
            fprintf ppf ": " ;
            if is_complex t then fprintf ppf "@ " ;
            aux ppf t ;
            fprintf ppf ";" )
          fl ;
        fprintf ppf "@]@ >"
    | DT_prop (_, _) ->
        let p, t = consume_outer_props t in
        fprintf ppf "%a%a" aux t props p
    | DT_var i -> fprintf ppf "$%i" i
  and props ppf p =
    fprintf ppf " [@prop {" ;
    let first = ref true in
    List.iter
      (fun (k, v) ->
        if !first then first := false else fprintf ppf "; " ;
        if v = "" then fprintf ppf "%s" k else fprintf ppf "%s = %S" k v )
      p ;
    fprintf ppf "}]"
  and tlist sep ppf = function
    | [] -> fprintf ppf "[]"
    | [x] -> aux ppf x
    | hd :: tl as l ->
        fprintf ppf "(" ;
        if List.exists is_complex l then fprintf ppf "@[<v>"
        else fprintf ppf "@[<h>" ;
        aux ppf hd ;
        List.iter
          (fun x ->
            ( match sep with
            | STAR -> fprintf ppf "@ *@ "
            | COMMA -> fprintf ppf ",@ " ) ;
            aux ppf x )
          tl ;
        fprintf ppf "@])"
  and name ppf n =
    match n.rec_args with
    | [] -> fprintf ppf "%s" n.rec_name
    | l -> fprintf ppf "%a %s" (tlist COMMA) l n.rec_name
  and descr ppf = function
    | DT_variant v ->
        let simple =
          List.for_all
            (fun (_, _, args) -> args = C_tuple [])
            v.variant_constrs
        in
        if simple then fprintf ppf "@[<h>" ;
        let first = ref simple in
        List.iter
          (fun (c, p, args) ->
            if not !first then fprintf ppf "@ | "
            else (
              fprintf ppf "@ " ;
              first := false ) ;
            fprintf ppf "%s" c ;
            if p <> [] then props ppf p ;
            let args = uninline args in
            if args <> [] then (
              fprintf ppf " of" ;
              if List.exists is_complex args then fprintf ppf "@ " ;
              fprintf ppf " %a" (tlist STAR) args ) )
          v.variant_constrs ;
        if simple then fprintf ppf "@]"
    | DT_record r ->
        fprintf ppf "@ {@[<v1>" ;
        List.iter
          (fun (c, p, arg) ->
            fprintf ppf "@ %s" c ;
            if p <> [] then props ppf p ;
            fprintf ppf ": " ;
            if is_complex arg then fprintf ppf "@ " ;
            aux ppf arg ;
            fprintf ppf ";" )
          r.record_fields ;
        fprintf ppf "@]@ }"
  in
  aux ppf

let print_hide_enumerations = print_stype ~show_enumerations:false
let print_ref = ref (print_stype ~show_enumerations:true)
let print ppf = !print_ref ppf

module Internal = struct
  let dummy_descr =
    DT_variant {variant_constrs= []; variant_repr= Variant_regular}

  let uid = ref 0

  let create_node name args =
    incr uid ;
    { rec_descr= dummy_descr
    ; rec_uid= !uid
    ; rec_name= name
    ; rec_args= args
    ; rec_has_var= None
    ; rec_hash= 0
    ; rec_memoized= [||] }

  let set_node_variant n (variant_constrs, variant_repr) =
    n.rec_descr <- DT_variant {variant_constrs; variant_repr}

  let set_node_record n (record_fields, record_repr) =
    n.rec_descr <- DT_record {record_fields; record_repr}

  let map_node ~ignore_props aux memo n =
    let prop = if ignore_props then fun _p -> [] else fun p -> p in
    let n' = create_node n.rec_name (List.map aux n.rec_args) in
    memo n' ;
    n'.rec_descr
    <- ( match n.rec_descr with
       | DT_variant v ->
           let variant_constrs =
             List.map
               (fun (s, p, args) ->
                 let args =
                   match args with
                   | C_tuple tl -> C_tuple (List.map aux tl)
                   | C_inline t -> C_inline (aux t)
                 in
                 (s, prop p, args) )
               v.variant_constrs
           in
           DT_variant {v with variant_constrs}
       | DT_record r ->
           let record_fields =
             List.map (fun (s, p, t) -> (s, prop p, aux t)) r.record_fields
           in
           DT_record {r with record_fields} ) ;
    n'

  let create_variant_type name args f =
    let n = create_node name args in
    let t = DT_node n in
    set_node_variant n (f t) ;
    t

  let create_record_type name args f =
    let n = create_node name args in
    let t = DT_node n in
    set_node_record n (f t) ;
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
      let h =
        ( name ~ignore_path:true n
        , List.map hash0 n.rec_args
        , String.concat ","
            ( match n.rec_descr with
            | DT_variant v -> List.map (fun (s, _, _) -> s) v.variant_constrs
            | DT_record r -> List.map (fun (s, _, _) -> s) r.record_fields ) )
        |> Hashtbl.hash
      in
      n.rec_hash <- h ;
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
      id1 = id2
      || hash0_node n1 = hash0_node n2
         &&
         let k = if id1 < id2 then (id1, id2) else (id2, id1) in
         try Hashtbl.find memo k with Not_found ->
           name ~ignore_path n1 = name ~ignore_path n2
           && equal_list geq n1.rec_args n2.rec_args
           &&
           ( (* Coinductive algorithm: first assume that the two types
             are equivalent. If we discover some difference, remember that
             they are different. At the end, we remove all the equalities
             that might depend on a false initial assumption. *)
             Hashtbl.replace memo k true ;
             stack := k :: !stack ;
             ( match (n1.rec_descr, n2.rec_descr) with
             | DT_variant v1, DT_variant v2 ->
                 let var_args_eq args1 args2 =
                   match (args1, args2) with
                   | C_tuple tl1, C_tuple tl2 -> equal_list geq tl1 tl2
                   | C_inline t1, C_inline t2 -> geq t1 t2
                   | _ -> false
                 in
                 let constr (c1, p1, tl1) (c2, p2, tl2) =
                   c1 = c2 && (ignore_props || p1 = p2) && var_args_eq tl1 tl2
                 in
                 equal_list constr v1.variant_constrs v2.variant_constrs
             | DT_record r1, DT_record r2 ->
                 let field (f1, p1, t1) (f2, p2, t2) =
                   f1 = f2 && (ignore_props || p1 = p2) && geq t1 t2
                 in
                 equal_list field r1.record_fields r2.record_fields
             | _ -> false )
             ||
             ( Hashtbl.replace memo k false ;
               false ) )
    and geq t1 t2 =
      t1 == t2
      ||
      match (t1, t2) with
      | (DT_prop (_, t1), t2 | t1, DT_prop (_, t2)) when ignore_props ->
          geq t1 t2
      | DT_node n1, DT_node n2 -> eq n1 n2
      | DT_tuple tl1, DT_tuple tl2 -> equal_list geq tl1 tl2
      | DT_list t1, DT_list t2
       |DT_array t1, DT_array t2
       |DT_option t1, DT_option t2 ->
          geq t1 t2
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
      if (not r) && !stack != [] then (
        List.iter
          (function k -> if Hashtbl.find memo k then Hashtbl.remove memo k)
          !stack ;
        stack := [] ) ;
      r

  (* Normalization functions based on some equality *)

  let normalizer eq =
    let module H = Hashtbl.Make (struct
      type t = node

      let equal n1 n2 = eq (DT_node n1) (DT_node n2)
      let hash = hash0_node
    end) in
    let memo = H.create 128 in
    fun t -> try H.find memo t with Not_found -> H.replace memo t t ; t

  let normalizer_node ~ignore_props ~ignore_path =
    normalizer (equal ~ignore_props ~ignore_path)

  (* Check if a stype has any substituable variable. *)

  let has_var t =
    let varstack = ref [] in
    let rec node = function
      | {rec_has_var= Some b; _} -> b
      | n ->
          if List.exists aux n.rec_args then (
            n.rec_has_var <- Some true ;
            true )
          else (
            n.rec_has_var <- Some false ;
            varstack := n :: !varstack ;
            ( match n.rec_descr with
            | DT_variant v ->
                List.exists
                  (fun (_, _, tl) -> List.exists aux (uninline tl))
                  v.variant_constrs
            | DT_record r ->
                List.exists (fun (_, _, t) -> aux t) r.record_fields )
            &&
            ( n.rec_has_var <- Some true ;
              true ) )
    and aux = function
      | DT_var _ -> true
      | DT_int | DT_float | DT_string -> false
      | DT_prop (_, t) | DT_list t | DT_array t | DT_option t -> aux t
      | DT_abstract (_, tl) | DT_tuple tl -> List.exists aux tl
      | DT_arrow (_, t1, t2) -> aux t1 || aux t2
      | DT_object fl -> List.exists (fun (_, t) -> aux t) fl
      | DT_node n -> node n
    in
    let r = aux t in
    if r then
      List.iter
        (function
          | {rec_has_var= Some false; _} as n -> n.rec_has_var <- None
          | _ -> ())
        !varstack ;
    r

  let node_has_var = function
    | {rec_has_var= Some b; _} -> b
    | n -> has_var (DT_node n)

  (* Substitution *)

  let debug = try Sys.getenv "DEBUG_MLFI_TYPES" <> "" with Not_found -> false

  let substitute subst t =
    if debug then (
      Format.printf "  SUBST t=%a@." print t ;
      Array.iteri
        (fun i _t -> Format.printf " SUBST %i -> %a@." i print subst.(i))
        subst ) ;
    let tbl = Hashtbl.create 4 in
    let rec aux = function DT_var i -> subst.(i) | t -> nonrec_map node aux t
    and node n =
      if not (node_has_var n) then n
      else
        try Hashtbl.find tbl n.rec_uid with Not_found ->
          if debug then Format.printf " SUBST #%i@." n.rec_uid ;
          map_node ~ignore_props:false aux (Hashtbl.replace tbl n.rec_uid) n
    in
    aux t

  (* Normalization *)

  let mapper ~ignore_props f =
    let tbl = Hashtbl.create 4 in
    let rec node aux n =
      try Hashtbl.find tbl n.rec_uid with Not_found ->
        let n1 = f n in
        if n != n1 then (
          let n2 = node aux n1 in
          Hashtbl.replace tbl n.rec_uid n2 ;
          n2 )
        else map_node ~ignore_props aux (Hashtbl.replace tbl n.rec_uid) n
    in
    gmap ~ignore_props node

  let normalize ~ignore_props ~ignore_path =
    mapper ~ignore_props (normalizer_node ~ignore_props ~ignore_path)

  let remove_props = mapper ~ignore_props:true (fun n -> n)
  let set_memoized n a = n.rec_memoized <- a
end

(* Textual form for import-export *)

module Textual = struct
  type stype = t
  type t = int gtype

  type node =
    | Variant of
        string
        * t list
        * (string * properties * t variant_args) list
        * variant_repr
    | Record of string * t list * (string * properties * t) list * record_repr

  type textual = {nodes: node array; t: t}

  module StringShare = Weak.Make (struct
    type t = string

    let hash = Hashtbl.hash
    let equal = ( = )
  end)

  let string_share = StringShare.create 512
  let share s = StringShare.merge string_share s
  let share_props l = List.map (fun (k, v) -> (share k, share v)) l

  let export_ t =
    let txt_nodes = Hashtbl.create 8 and counter = ref 0 in
    let nodes = Hashtbl.create 8 in
    let tbl = Hashtbl.create 4 in
    let node aux n : int =
      try Hashtbl.find tbl n.rec_uid with Not_found ->
        let args = List.map aux n.rec_args in
        let name = n.rec_name in
        let id = !counter in
        incr counter ;
        Hashtbl.replace tbl n.rec_uid id ;
        let d =
          match n.rec_descr with
          | DT_variant v ->
              let constrs =
                List.map
                  (fun (s, p, args) ->
                    ( share s
                    , share_props p
                    , match args with
                      | C_tuple tl -> C_tuple (List.map aux tl)
                      | C_inline t -> C_inline (aux t) ) )
                  v.variant_constrs
              in
              Variant (name, args, constrs, v.variant_repr)
          | DT_record r ->
              let fields =
                List.map
                  (fun (s, p, t) -> (share s, share_props p, aux t))
                  r.record_fields
              in
              Record (name, args, fields, r.record_repr)
        in
        Hashtbl.add nodes id n ; Hashtbl.add txt_nodes id d ; id
    in
    let t = gmap ~ignore_props:false node t in
    ( {nodes= Array.init !counter (Hashtbl.find txt_nodes); t}
    , Array.init !counter (Hashtbl.find nodes) )

  let export t = fst (export_ t)

  let export_with_digests t =
    let txt, nodes = export_ t in
    let digest n =
      let tbl = export (DT_node n) in
      Digest.string (Marshal.to_string tbl [Marshal.No_sharing])
    in
    let digests = Array.map digest nodes in
    assert (Array.length digests = Array.length txt.nodes) ;
    (txt, digests)

  let digest2node = Hashtbl.create 256

  let import d digests =
    let nodes = Array.make (Array.length d.nodes) None in
    let really_node aux i =
      match d.nodes.(i) with
      | Variant (name, args, constrs, repr) ->
          unnode
            (Internal.create_variant_type name (List.map aux args) (fun node ->
                 nodes.(i) <- Some (unnode node) ;
                 ( List.map
                     (fun (s, p, args) ->
                       ( share s
                       , share_props p
                       , match args with
                         | C_tuple tl -> C_tuple (List.map aux tl)
                         | C_inline t -> C_inline (aux t) ) )
                     constrs
                 , repr ) ))
      | Record (name, args, fields, repr) ->
          unnode
            (Internal.create_record_type name (List.map aux args) (fun node ->
                 nodes.(i) <- Some (unnode node) ;
                 ( List.map
                     (fun (s, p, t) -> (share s, share_props p, aux t))
                     fields
                 , repr ) ))
    in
    let node aux i =
      match nodes.(i) with
      | Some v -> v
      | None ->
          if i < Array.length digests then (
            let digest = digests.(i) in
            try
              let r = Hashtbl.find digest2node digest in
              nodes.(i) <- Some r ; r
            with Not_found ->
              let r = really_node aux i in
              Hashtbl.replace digest2node digest r ;
              r )
          else really_node aux i
    in
    gmap ~ignore_props:false node d.t

  let import_table txt digests =
    assert (Array.length txt.nodes = Array.length digests) ;
    match import txt digests with
    | DT_tuple l -> Array.of_list l
    | _ -> assert false

  let import t = import t [||]
end

let strict_equality = Internal.equal ~ignore_props:false ~ignore_path:false
let equality_modulo_props = Internal.equal ~ignore_props:true ~ignore_path:true
let equality = Internal.equal ~ignore_props:false ~ignore_path:true

let strict_equality t1 t2 =
  match (t1, t2) with
  | DT_node n1, DT_node n2 ->
      n1 == n2 || (n1.rec_name = n2.rec_name && strict_equality t1 t2)
  | DT_node _, _ | _, DT_node _ -> false
  | _ -> strict_equality t1 t2
