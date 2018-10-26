(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

type record_repr = Regular | Float | Unboxed
type constr_repr = Tag of int | Unboxed

type 'a t = {t: 'a Ttype.t; xt: 'a xtype Lazy.t}

and 'a xtype =
  | Unit : unit xtype
  | Bool : bool xtype
  | Int : int xtype
  | Float : float xtype
  | String : string xtype
  | Char : char xtype
  | Int32 : int32 xtype
  | Int64 : int64 xtype
  | Nativeint : nativeint xtype
  | List : 'b t -> 'b list xtype
  | Option : 'b t -> 'b option xtype
  | Array : 'b t -> 'b array xtype
  | Lazy : 'b t -> 'b Lazy.t xtype
  | Tuple : 'a tuple -> 'a xtype
  | Record : 'a record -> 'a xtype
  | Sum : 'a sum -> 'a xtype
  | Function : ('b, 'c) arrow -> ('b -> 'c) xtype
  | Object : 'a object_ -> 'a xtype
  | Prop : (Stype.properties * 'a t) -> 'a xtype
  | Abstract : (string * Stype.t list) -> 'a xtype

and ('s, 't) element = {typ: 't t; nth: int}

and 's field = Field : ('s, 't) element -> 's field

and 's tuple = {t_flds: 's field list; t_len: int}

and label = string * Stype.properties

and 's record_field = label * 's field

and 's record =
  { r_flds: 's record_field list
  ; r_len: int
  ; r_repr: record_repr
  ; r_lookup: string -> 's record_field option }

and 's constant_constructor = {cc_label: label; cc_nr: int}

and ('s, 't) regular_constructor =
  {rc_label: label; rc_flds: 't field list; rc_len: int; rc_repr: constr_repr}

and ('s, 't) inlined_constructor =
  { ic_label: label
  ; ic_flds: 't record_field list
  ; ic_len: int
  ; ic_repr: constr_repr
  ; ic_lookup: string -> 't record_field option }

and 's constructor =
  | Constant : 's constant_constructor -> 's constructor
  | Regular : ('s, 't) regular_constructor -> 's constructor
  | Inlined : ('s, 't) inlined_constructor -> 's constructor

and 's sum =
  { s_cstrs: 's constructor list
  ; s_lookup: string -> 's constructor option
  ; s_cstr_by_value: 's -> 's constructor }

and ('s, 't) arrow = {arg_label: string option; arg_t: 's t; res_t: 't t}

and 's method_ = Method : string * ('s, 't) element -> 's method_

and 's object_ =
  {o_methods: 's method_ list; o_lookup: string -> 's method_ option}

(* internal Helpers *)
let cast_ttype : Stype.t -> 'a Ttype.t = Obj.magic
let cast_xtype : type a b. a xtype -> b xtype = Obj.magic

module StepMeta = Path.Internal [@@ocaml.warning "-3"]

(* Memoize xtype in stype node *)
module M = struct
  open Stype

  type memoized_type_prop += Xtype of Obj.t xtype

  let rec search a n i =
    if i = n then None
    else match a.(i) with Xtype r -> Some r | _ -> search a n (i + 1)

  let is_memoized (node : node) : Obj.t xtype option =
    search node.rec_memoized (Array.length node.rec_memoized) 0

  let memoize : type a. node -> a xtype -> a xtype =
   fun node xt ->
    let s = Xtype (Obj.magic xt) in
    let old = node.rec_memoized in
    let a = Array.make (Array.length old + 1) s in
    Array.blit old 0 a 1 (Array.length old) ;
    Internal.set_memoized node a ;
    xt
end

(* prepare fast lookup for named elements *)
let lookup : ('a -> string) -> 'a list -> string -> 'a option =
 fun get_name lst ->
  let tbl = lazy (Ext.String.Tbl.prepare (List.map get_name lst))
  and arr = lazy (Array.of_list lst) in
  fun name ->
    let i = Ext.String.Tbl.lookup (Lazy.force tbl) name in
    if i < 0 then None else Some (Lazy.force arr).(i)

let cstr_table : type a. a constructor list -> a constructor array * (a -> int)
    =
 fun cstrs ->
  let cst, ncst = (ref [], ref []) in
  List.iteri
    (fun i -> function Constant _ -> cst := i :: !cst
      | Regular _ | Inlined _ -> ncst := i :: !ncst )
    cstrs ;
  let cstrs = Array.of_list cstrs in
  let cst = Ext.Array.of_list_rev !cst in
  let ncst = Ext.Array.of_list_rev !ncst in
  let fnd x =
    let x = Obj.repr x in
    if Obj.is_int x then cst.(Obj.magic x) else ncst.(Obj.tag x)
  in
  (cstrs, fnd)

(* prepare finding a constructor by value *)
let cstr_by_value : type a. a constructor list -> a -> a constructor =
 fun cstrs ->
  let aux = lazy (cstr_table cstrs) in
  fun x ->
    let cstrs, fnd = Lazy.force aux in
    cstrs.(fnd x)

let rec xtype_of_ttype : type a. a Ttype.t -> a xtype =
 fun t ->
  (* CAUTION: This must be consistent with core/std.ml *)
  match Ttype.to_stype t with
  | DT_int -> cast_xtype Int
  | DT_float -> cast_xtype Float
  | DT_string -> cast_xtype String
  | DT_abstract ("unit", []) -> cast_xtype Unit
  | DT_abstract ("bool", []) -> cast_xtype Bool
  | DT_abstract ("char", []) -> cast_xtype Char
  | DT_abstract ("int32", []) -> cast_xtype Int32
  | DT_abstract ("int64", []) -> cast_xtype Int64
  | DT_abstract ("nativeint", []) -> cast_xtype Nativeint
  | DT_option t -> cast_xtype (Option (bundle t))
  | DT_list t -> cast_xtype (List (bundle t))
  | DT_array t -> cast_xtype (Array (bundle t))
  | DT_abstract ("Lazy.t", [t]) -> cast_xtype (Lazy (bundle t))
  | DT_arrow (l, t1, t2) ->
      let arg_label = match l with "" -> None | s -> Some s in
      cast_xtype (Function {arg_label; arg_t= bundle t1; res_t= bundle t2})
  | DT_tuple l -> cast_xtype (Tuple {t_flds= fields l; t_len= List.length l})
  | DT_node ({rec_descr= DT_record r; _} as node) -> (
    match M.is_memoized node with
    | None -> M.memoize node (cast_xtype (Record (record r)))
    | Some xt -> cast_xtype xt )
  | DT_node ({rec_descr= DT_variant v; _} as node) -> (
    match M.is_memoized node with
    | None -> M.memoize node (cast_xtype (Sum (sum v)))
    | Some xt -> cast_xtype xt )
  | DT_object methods -> cast_xtype (Object (object_ methods))
  | DT_prop (props, s) -> Prop (props, bundle s)
  | DT_abstract (name, l) -> Abstract (name, l)
  | DT_var _ -> assert false

and bundle : type a. Stype.t -> a t =
 fun s ->
  let t = cast_ttype s in
  {t; xt= lazy (xtype_of_ttype t)}

and fields (fields : Stype.t list) : 'a field list =
  List.mapi (fun i t -> Field {typ= bundle t; nth= i}) fields

and record_fields fields : 'a record_field list =
  List.mapi
    (fun nth (field_name, field_props, s) ->
      ((field_name, field_props), Field {typ= bundle s; nth}) )
    fields

and record (r : Stype.record_descr) : 'a record =
  let r_flds = record_fields r.record_fields in
  let r_repr : record_repr =
    match r.record_repr with
    | Record_unboxed -> Unboxed
    | Record_float -> Float
    | Record_regular -> Regular
    | Record_inline _ -> assert false
  and r_len = List.length r.record_fields
  and r_lookup = lookup (fun f -> fst (fst f)) r_flds in
  {r_flds; r_repr; r_len; r_lookup}

and sum (v : Stype.variant_descr) : 'a sum =
  let repr =
    match v.variant_repr with
    | Variant_unboxed -> fun _ -> Unboxed
    | Variant_regular -> fun tag -> Tag tag
  in
  let pincr i =
    let r = !i in
    incr i ; r
  in
  (* post increment *)
  let cst_i, ncst_i = (ref 0, ref 0) in
  let s_cstrs =
    List.map
      (fun (name, props, arg) ->
        let lbl = (name, props) in
        let open Stype in
        match arg with
        | C_tuple [] -> Constant {cc_label= lbl; cc_nr= pincr cst_i}
        | C_tuple l ->
            let tag = pincr ncst_i in
            Regular
              { rc_label= lbl
              ; rc_flds= fields l
              ; rc_len= List.length l
              ; rc_repr= repr tag }
        | C_inline (DT_node {rec_descr= DT_record r; _}) ->
            let tag = pincr ncst_i in
            let ic_flds = record_fields r.record_fields in
            Inlined
              { ic_label= lbl
              ; ic_flds
              ; ic_len= List.length r.record_fields
              ; ic_repr= repr tag
              ; ic_lookup= lookup (fun f -> fst (fst f)) ic_flds }
        | C_inline _ -> assert false )
      v.variant_constrs
  in
  let s_lookup =
    lookup
      (function
        | Constant {cc_label= name, _; _}
         |Regular {rc_label= name, _; _}
         |Inlined {ic_label= name, _; _} ->
            name)
      s_cstrs
  and s_cstr_by_value = cstr_by_value s_cstrs in
  {s_cstrs; s_lookup; s_cstr_by_value}

and object_ methods : 'a object_ =
  let prepare (type a) nth (name, stype) =
    let typ : a t = bundle stype in
    Method (name, {typ; nth})
  in
  let o_methods = List.mapi prepare methods in
  let o_lookup = lookup (function Method (name, _) -> name) o_methods in
  {o_methods; o_lookup}

let of_ttype t = {t; xt= lazy (xtype_of_ttype t)}

let rec remove_outer_props : 'a t -> 'a t =
 fun t ->
  match Lazy.force t.xt with Prop (_, t) -> remove_outer_props t | _ -> t

let rec consume_outer_props acc t =
  match Lazy.force t.xt with
  | Prop (p, t) -> consume_outer_props (p @ acc) t
  | _ -> (acc, t)

let consume_outer_props : 'a t -> Stype.properties * 'a t =
 fun t -> consume_outer_props [] t

(* builders *)

module Builder = struct
  type 'a t = {mk: 't. ('a, 't) element -> 't} [@@unboxed]
  type 's t' = {mk: 't. label -> ('s, 't) element -> 't} [@@unboxed]

  let fields : type a. int -> a field list -> a t -> a =
   fun arity flds b ->
    let o = Obj.new_block 0 arity in
    List.iteri
      (fun i -> function Field f -> Obj.set_field o i (Obj.repr (b.mk f)))
      flds ;
    Obj.magic o

  let record_fields : type a. int -> a record_field list -> a t' -> a =
   fun arity flds b ->
    let o = Obj.new_block 0 arity in
    List.iteri
      (fun i -> function
        | l, Field f -> Obj.set_field o i (Obj.repr (b.mk l f)) )
      flds ;
    Obj.magic o

  let tuple t = fields t.t_len t.t_flds

  let record : type a. a record -> a t' -> a =
   fun {r_flds; r_repr; r_len; _} b ->
    match r_repr with
    | Regular -> record_fields r_len r_flds b
    | Float ->
        let o = Obj.new_block Obj.double_array_tag r_len in
        List.iteri
          (fun i -> function
            | l, Field f -> Obj.set_double_field o i (Obj.magic (b.mk l f)) )
          r_flds ;
        Obj.magic o
    | Unboxed -> (
      match r_flds with
      | [(l, Field f)] -> Obj.magic (b.mk l f)
      | _ -> assert false )

  let constant_constructor : type a. a constant_constructor -> a =
   fun {cc_nr; _} -> Obj.magic cc_nr

  let regular_constructor : type a b. (a, b) regular_constructor -> b t -> a =
   fun {rc_flds; rc_repr; rc_len; _} b ->
    match (rc_repr, rc_flds) with
    | Tag tag, _ ->
        let o = Obj.repr (fields rc_len rc_flds b) in
        Obj.set_tag o tag ; Obj.magic o
    | Unboxed, [Field f] -> Obj.magic (b.mk f)
    | Unboxed, _ -> assert false

  let inlined_constructor : type a b. (a, b) inlined_constructor -> b t' -> a =
   fun {ic_flds; ic_repr; ic_len; _} b ->
    match (ic_repr, ic_flds) with
    | Tag tag, _ ->
        let o = Obj.repr (record_fields ic_len ic_flds b) in
        Obj.set_tag o tag ; Obj.magic o
    | Unboxed, [(l, Field f)] -> Obj.magic (b.mk l f)
    | Unboxed, _ -> assert false

  type generic = {mk: 's 't. ('s, 't) element -> 't} [@@unboxed]

  let constructor : type a. a constructor -> generic -> a =
   fun c b ->
    match c with
    | Constant c -> constant_constructor c
    | Regular c -> regular_constructor c {mk= b.mk}
    | Inlined c -> inlined_constructor c {mk= (fun _ -> b.mk)}
end

module Assembler = struct
  module Reader : sig
    (* From mlfi_json. *)
    type 'a t

    val mk : (string * 'a) list -> 'a t
    val get : 'a t -> string -> 'a
  end = struct
    type 'a t = (string * 'a) list ref

    let mk l = ref l

    (* Optimized lookup for record fields, for the case where the ordering
       match between the JSON value and the OCaml record definition. *)

    let get (r : 'a t) s =
      match !r with
      | (t, v) :: rest when t = s ->
          r := rest ;
          v
      | [] -> raise Not_found
      | _ :: rest ->
          (* TODO: fallback to building a lookup table here... *)
          List.assoc s rest
  end

  (* TODO: Instead of the Xtype.t, this could also pass an element. *)
  type 'a asm = {f: 'b. 'b t -> 'a -> 'b} [@@unboxed]

  let fun_arr_of_flds arity flds asm =
    let flds = ref flds in
    Array.init arity (fun _i ->
        match !flds with
        | [] -> assert false
        | Field el :: tl ->
            flds := tl ;
            let f = asm.f el.typ in
            Obj.magic f )

  let fun_arr_of_flds' arity flds asm =
    let flds = ref flds in
    Array.init arity (fun _i ->
        match !flds with
        | [] -> assert false
        | (_, Field el) :: tl ->
            flds := tl ;
            let f = asm.f el.typ in
            Obj.magic f )

  let init : int -> int -> 'a field list -> 'b asm -> 'b list -> Obj.t =
   fun tag arity flds asm ->
    let farr = lazy (fun_arr_of_flds arity flds asm) in
    fun lst ->
      let farr = Lazy.force farr in
      let o = Obj.new_block tag arity in
      let lst = ref lst in
      for i = 0 to arity - 1 do
        let hd =
          match !lst with
          (* TODO: introduce exception or use result as return type *)
          | [] -> failwith "Xtype.Assembler.tuple: list too short"
          | hd :: tl ->
              lst := tl ;
              hd
        in
        Obj.set_field o i (farr.(i) hd)
      done ;
      o

  let init' 
      :  int
      -> int
      -> 'a record_field list
      -> 'b asm
      -> (string * 'b) list
      -> Obj.t =
   fun tag arity flds asm ->
    let farr = lazy (fun_arr_of_flds' arity flds asm) in
    fun lst ->
      let farr = Lazy.force farr in
      let o = Obj.new_block tag arity in
      let rd = Reader.mk lst in
      List.iteri
        (fun i ((name, _), _) ->
          let b =
            try
              Reader.get rd name
              (* TODO: introduce exception or use result as return type *)
            with Not_found -> failwith "Xtype.Assembler.record: field missing"
          in
          Obj.set_field o i (farr.(i) b) )
        flds ;
      o

  let tuple : 'a tuple -> 'b asm -> 'b list -> 'a =
   fun tup asm ->
    let init : 'b list -> Obj.t = init 0 tup.t_len tup.t_flds asm in
    Obj.magic init

  let record : 'a record -> 'b asm -> (string * 'b) list -> 'a =
   fun r asm ->
    if r.r_repr = Unboxed then failwith "TODO: unboxed records" ;
    let init : (string * 'b) list -> Obj.t = init' 0 r.r_len r.r_flds asm in
    Obj.magic init

  type ('a, 'b) cstr =
    | Constant : 'a constant_constructor -> ('a, 'b) cstr
    | Regular : ('a, 'c) regular_constructor * 'b list -> ('a, 'b) cstr
    | Inlined :
        ('a, 'c) inlined_constructor * (string * 'b) list
        -> ('a, 'b) cstr

  let sum : 'a sum -> 'b asm -> ('a, 'b) cstr -> 'a =
   fun sum asm ->
    let aux =
      lazy
        ((* TODO: store number of constructors in stype *)
         let rev_inlined, rev_regular =
           List.fold_left
             (fun (inlined, regular) c ->
               match (c : _ constructor) with
               | Constant _ -> (inlined, regular)
               | Regular c ->
                   let init =
                     match c.rc_repr with
                     | Unboxed -> failwith "TODO: support unboxed"
                     | Tag tag -> init tag c.rc_len c.rc_flds asm
                   in
                   (None :: inlined, Some init :: regular)
               | Inlined c ->
                   let init =
                     match c.ic_repr with
                     | Unboxed -> failwith "TODO: support unboxed"
                     | Tag tag -> init' tag c.ic_len c.ic_flds asm
                   in
                   (Some init :: inlined, None :: regular) )
             ([], []) sum.s_cstrs
         in
         (Ext.Array.of_list_rev rev_inlined, Ext.Array.of_list_rev rev_regular))
    in
    fun c ->
      let inlined, regular = Lazy.force aux in
      match c with
      | Constant c -> Obj.magic c.cc_nr
      | Regular (c, l) -> (
        match c.rc_repr with
        | Unboxed -> failwith "TODO: unboxed constructors"
        | Tag i -> (
          match regular.(i) with
          | None -> assert false
          | Some f -> Obj.magic (f l : Obj.t) ) )
      | Inlined (c, l) -> (
        match c.ic_repr with
        | Unboxed -> failwith "TODO: unboxed constructors"
        | Tag i -> (
          match inlined.(i) with
          | None -> assert false
          | Some f -> Obj.magic (f l : Obj.t) ) )
end

module Make = struct
  type 'a t = Obj.t option array

  exception Missing_field of string

  let set : 'a t -> ('a, 'b) element -> 'b -> unit =
   fun arr {nth; _} x -> arr.(nth) <- Some (Obj.repr x)

  (* Reuse Builder *)

  let builder : ('a t -> unit) -> int -> 'a Builder.t =
   fun init n ->
    let arr = Array.make n None in
    let mk {nth; _} =
      match arr.(nth) with
      | None -> raise (Missing_field (string_of_int nth))
      | Some o -> Obj.magic o
    in
    init arr ; {mk}

  let named_builder : ('a t -> unit) -> int -> 'a Builder.t' =
   fun init n ->
    let arr = Array.make n None in
    let mk _ {nth; _} =
      match arr.(nth) with
      | None -> raise (Missing_field (string_of_int nth))
      | Some o -> Obj.magic o
    in
    init arr ; {mk}

  let tuple : 'a tuple -> ('a t -> unit) -> 'a =
   fun {t_flds; t_len} f -> Builder.fields t_len t_flds (builder f t_len)

  let record : 'a record -> ('a t -> unit) -> 'a =
   fun r f -> Builder.record r (named_builder f r.r_len)

  let regular_constructor : type a b.
      (a, b) regular_constructor -> (b t -> unit) -> a =
   fun c f -> Builder.regular_constructor c (builder f c.rc_len)

  let inlined_constructor : type a b.
      (a, b) inlined_constructor -> (b t -> unit) -> a =
   fun c f -> Builder.inlined_constructor c (named_builder f c.ic_len)
end

module Read = struct
  let check tag o = Obj.is_block o && Obj.tag o = tag
  let cast : (Obj.t -> Obj.t) -> 'a -> 'b = Obj.magic

  let range n =
    let rec aux i acc = if i < 0 then acc else aux (i - 1) (i :: acc) in
    aux (n - 1) []

  let tuple : type a b. a tuple -> (a, b) element -> a -> b =
   fun _ el -> cast (fun o -> Obj.field o el.nth)

  let record : type a b. a record -> (a, b) element -> a -> b =
   fun r el ->
    cast
      ( match r.r_repr with
      | Regular -> fun o -> Obj.field o el.nth
      | Float -> fun o -> Obj.repr (Obj.double_field o el.nth)
      | Unboxed -> fun o -> Obj.repr o )

  let cast : (Obj.t -> Obj.t option) -> 'a -> 'b option = Obj.magic
  let checked tag i o = if check tag o then Some (Obj.field o i) else None

  let regular_constructor : type a b c.
      (a, b) regular_constructor -> (b, c) element -> a -> c option =
   fun c el ->
    cast
      ( match c.rc_repr with
      | Tag tag -> checked tag el.nth
      | Unboxed -> fun o -> Some (Obj.repr o) )

  let inlined_constructor : type a b c.
      (a, b) inlined_constructor -> (b, c) element -> a -> c option =
   fun c el ->
    cast
      ( match c.ic_repr with
      | Tag tag -> checked tag el.nth
      | Unboxed -> fun o -> Some (Obj.repr o) )

  let call_method : 'a object_ -> ('a, 'b) element -> 'a -> 'b =
   fun o e ->
    let (Method (name, _)) = List.nth o.o_methods e.nth in
    let label = CamlinternalOO.public_method_label name in
    fun x -> Obj.magic (CamlinternalOO.send (Obj.magic x) label)

  type 'b mapf = {f: 'a. 'a t -> 'a -> 'b} [@@unboxed]
  type 'b mapf' = {f: 'a. name:string -> 'a t -> 'a -> 'b} [@@unboxed]

  let fun_arr_of_fields (type a) n flds (mapf : a mapf) : (Obj.t -> a) array =
    let flds = ref flds in
    Array.init n (fun _i ->
        match !flds with
        | [] -> assert false
        | Field e :: tl ->
            flds := tl ;
            let f = mapf.f e.typ in
            fun (o : Obj.t) -> f (Obj.magic o) )

  let fun_arr_of_fields' (type a) n flds (mapf : a mapf') : (Obj.t -> a) array
      =
    let flds = ref flds in
    Array.init n (fun _i ->
        match !flds with
        | [] -> assert false
        | ((name, _), Field e) :: tl ->
            flds := tl ;
            let f = mapf.f ~name e.typ in
            fun (o : Obj.t) -> f (Obj.magic o) )

  let map_tuple (type a b) (tup : a tuple) (mapf : b mapf) =
    let farr = lazy (fun_arr_of_fields tup.t_len tup.t_flds mapf) in
    let range = range tup.t_len in
    fun (x : a) ->
      let farr = Lazy.force farr in
      List.map (fun i -> farr.(i) (Obj.field (Obj.repr x) i)) range

  let map_record (type a b) (r : a record) (mapf : b mapf') =
    let farr = lazy (fun_arr_of_fields' r.r_len r.r_flds mapf) in
    if r.r_repr <> Regular then failwith "TODO: unboxed/float records" ;
    let range = range r.r_len in
    fun (x : a) ->
      let farr = Lazy.force farr in
      List.map (fun i -> farr.(i) (Obj.field (Obj.repr x) i)) range

  type ('a, 'b) mapped_sum =
    | Regular of string * 'a list
    | Inlined of string * 'b list
    | Constant of string

  let map_sum : type a b c.
      a sum -> b mapf -> c mapf' -> a -> (b, c) mapped_sum =
   fun sum mapf mapf' ->
    let aux =
      lazy
        (let cstrs, fnd = cstr_table sum.s_cstrs in
         let farr =
           Array.init (Array.length cstrs) (fun i ->
               match cstrs.(i) with
               | Constant c -> fun _ -> Constant (fst c.cc_label)
               | Regular c ->
                   if c.rc_repr = Unboxed then failwith "TODO: unboxed cstr" ;
                   let farr' = fun_arr_of_fields c.rc_len c.rc_flds mapf in
                   let range = range c.rc_len in
                   fun o ->
                     Regular
                       ( fst c.rc_label
                       , List.map (fun i -> farr'.(i) (Obj.field o i)) range )
               | Inlined c ->
                   if c.ic_repr = Unboxed then failwith "TODO: unboxed cstr" ;
                   let farr' = fun_arr_of_fields' c.ic_len c.ic_flds mapf' in
                   let range = range c.ic_len in
                   fun o ->
                     Inlined
                       ( fst c.ic_label
                       , List.map (fun i -> farr'.(i) (Obj.field o i)) range )
           )
         in
         (farr, fnd))
    in
    fun (x : a) ->
      let farr, fnd = Lazy.force aux in
      farr.(fnd x) (Obj.repr x)
end

(* paths *)

module Step = struct
  let cast : (Obj.t, Obj.t) Path.lens -> ('a, 'b) Path.lens = Obj.magic
  let check tag o = Obj.is_block o && Obj.tag o = tag

  (* TODO: This could use the above Read module *)

  let tuple : 'a tuple -> ('a, 'b) element -> ('a, 'b) Path.step =
   fun tup f ->
    let nth = f.nth in
    let get o = Some (Obj.field o nth) in
    let set o v =
      let o = Obj.dup o in
      Obj.set_field o nth (Obj.repr v) ;
      Some o
    in
    (cast {get; set}, StepMeta.tuple ~nth ~arity:tup.t_len)

  let record : 'a record -> ('a, 'b) element -> ('a, 'b) Path.step =
   fun r f ->
    let i = f.nth in
    let (field_name, _), _ = List.nth r.r_flds i in
    let get, set =
      match r.r_repr with
      | Regular ->
          ( (fun r -> Some (Obj.field r i))
          , fun r v ->
              let r = Obj.dup r in
              Obj.set_field r i (Obj.repr v) ;
              Some r )
      | Float ->
          ( (fun r -> Some (Obj.repr (Obj.double_field r i)))
          , fun r v ->
              let r = Obj.dup r in
              Obj.set_double_field r i (Obj.magic v) ;
              Some r )
      | Unboxed -> ((fun r -> Some (Obj.repr r)), fun _r v -> Some (Obj.repr v))
    in
    (cast {get; set}, StepMeta.field ~field_name)

  let regular_constructor 
      : ('a, 'b) regular_constructor -> ('b, 'c) element -> ('a, 'c) Path.step
      =
   fun c f ->
    let arity = c.rc_len in
    let nth = f.nth in
    let get, set =
      match c.rc_repr with
      | Unboxed -> ((fun o -> Some o), fun _o v -> Some v)
      | Tag tag ->
          ( (fun o -> if check tag o then Some (Obj.field o nth) else None)
          , fun o v ->
              if check tag o then (
                let o = Obj.dup o in
                Obj.set_field o nth (Obj.repr v) ;
                Some o )
              else None )
    in
    let name = fst c.rc_label in
    (cast {get; set}, StepMeta.constructor_regular ~name ~nth ~arity)

  let inlined_constructor 
      : ('a, 'b) inlined_constructor -> ('b, 'c) element -> ('a, 'c) Path.step
      =
   fun c f ->
    let i = f.nth in
    let (field_name, _), _ = List.nth c.ic_flds i in
    let get, set =
      match c.ic_repr with
      | Tag tag ->
          ( (fun r -> if check tag r then Some (Obj.field r i) else None)
          , fun r v ->
              if check tag r then (
                let r = Obj.dup r in
                Obj.set_field r i (Obj.repr v) ;
                Some r )
              else None )
      | Unboxed -> ((fun r -> Some (Obj.repr r)), fun _r v -> Some (Obj.repr v))
    in
    let name = fst c.ic_label in
    (cast {get; set}, StepMeta.constructor_inline ~name ~field_name)
end

let option_step_some : type a. a Ttype.t -> (a option, a) Path.step =
 (* ttype helps to avoid Obj.magic *)
 fun _t ->
  let get x = x
  and set x v = match x with None -> None | Some _ -> Some (Some v) in
  (Path.{get; set}, StepMeta.constructor_regular ~name:"Some" ~nth:0 ~arity:1)

let rec all_paths : type a b. a Ttype.t -> b Ttype.t -> (a, b) Path.t list =
 fun root target ->
  match Ttype.equality root target with
  | Some TypEq.Eq -> [Path.[]]
  | None -> (
    match xtype_of_ttype root with
    | Unit -> []
    | Bool -> []
    | Int -> []
    | Float -> []
    | String -> []
    | Char -> []
    | Int32 -> []
    | Int64 -> []
    | Nativeint -> []
    | Option {t; _} ->
        let paths = all_paths t target in
        List.map Path.(fun p -> option_step_some t :: p) paths
    | Tuple t -> tuple ~target t
    | Record r -> record ~target r
    | Sum sum ->
        List.map
          (function
            | Constant _ -> []
            | Regular c -> regular_constructor ~target c
            | Inlined c -> inlined_constructor ~target c)
          sum.s_cstrs
        |> List.concat
    | Prop (_, {t; _}) -> all_paths t target
    | Object _ -> []
    | List _ -> []
    | Array _ -> []
    | Function _ -> []
    | Lazy _ -> []
    | Abstract _ -> [] )

and tuple : type a b. target:b Ttype.t -> a tuple -> (a, b) Path.t list =
 fun ~target tup ->
  List.map
    (function
      | Field f ->
          let paths = all_paths f.typ.t target in
          List.map Path.(fun p -> Step.tuple tup f :: p) paths)
    tup.t_flds
  |> List.concat

and record : type a b. target:b Ttype.t -> a record -> (a, b) Path.t list =
 fun ~target r ->
  List.map
    (function
      | _, Field f ->
          let paths = all_paths f.typ.t target in
          List.map Path.(fun p -> Step.record r f :: p) paths)
    r.r_flds
  |> List.concat

and regular_constructor : type a b c.
    target:c Ttype.t -> (a, b) regular_constructor -> (a, c) Path.t list =
 fun ~target c ->
  List.map
    (function
      | Field f ->
          let paths = all_paths f.typ.t target in
          List.map Path.(fun p -> Step.regular_constructor c f :: p) paths)
    c.rc_flds
  |> List.concat

and inlined_constructor : type a b c.
    target:c Ttype.t -> (a, b) inlined_constructor -> (a, c) Path.t list =
 fun ~target c ->
  List.map
    (function
      | _, Field f ->
          let paths = all_paths f.typ.t target in
          List.map Path.(fun p -> Step.inlined_constructor c f :: p) paths)
    c.ic_flds
  |> List.concat

(* Project ttype along path *)

let assert_some = function Some x -> x | None -> assert false
let cast : type a b. a t -> b Ttype.t = fun t -> Obj.magic t.t

let rec project_path : type a b. a Ttype.t -> (a, b) Path.t -> b Ttype.t =
 fun t ->
  let open Path in
  function
  | [] -> t
  | (_, meta) :: tl ->
      let t =
        match (meta, xtype_of_ttype t) with
        | Field {field_name}, Record r -> (
            r.r_lookup field_name |> assert_some
            |> function _, Field f -> cast f.typ )
        | Tuple {nth; _}, Tuple t -> (
            List.nth t.t_flds nth |> function Field f -> cast f.typ )
        | List _, List t -> cast t
        | Array _, Array t -> cast t
        | Constructor {name; arg}, Sum s -> (
          match (arg, s.s_lookup name |> assert_some) with
          | Regular {nth; _}, Regular c -> (
              List.nth c.rc_flds nth |> function Field f -> cast f.typ )
          | Inline {field_name}, Inlined c -> (
              c.ic_lookup field_name |> assert_some
              |> function _, Field f -> cast f.typ )
          | Regular _, _ | Inline _, _ -> assert false )
        | _ -> assert false
      in
      project_path t tl
