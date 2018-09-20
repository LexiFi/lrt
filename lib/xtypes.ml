open Dynt_core
open Dynt_core.Ttype
open Dynt_core.Stype

type record_repr = Regular | Float | Unboxed
type constr_repr = Tag of int | Unboxed

type 'a t =
  { t: 'a ttype
  ; xt: 'a xtype Lazy.t
  }

and 'a xtype =
  | Unit: unit xtype
  | Bool: bool xtype
  | Int: int xtype
  | Float: float xtype
  | String: string xtype
  | Char: char xtype
  | Int32: int32 xtype
  | Int64: int64 xtype
  | Nativeint: nativeint xtype
  | Option: 'b t -> 'b option xtype
  | List: 'b t -> 'b list xtype
  | Array: 'b t -> 'b array xtype
  | Lazy: 'b t -> 'b Lazy.t xtype
  | Tuple: 'a tuple -> 'a xtype
  | Record: 'a record -> 'a xtype
  | Sum: 'a sum -> 'a xtype
  | Function: ('b,'c) arrow -> ('b -> 'c) xtype
  | Object: 'a object_ -> 'a xtype
  | Prop: (stype_properties * 'a t) -> 'a xtype
  | Abstract: (string * stype list) -> 'a xtype

and ('s,'t) element =
  { typ: 't t
  ; nth: int
  }

and 's field =
  | Field: ('s, 't) element -> 's field

and 's tuple =
  { t_flds : 's field list }

and label = string * stype_properties

and 's record_field = label * 's field

and 's record =
  { r_flds: 's record_field list
  ; r_repr: record_repr
  }

and 's constant_constructor =
  { cc_label: label
  ; cc_nr: int
  }

and ('s, 't) regular_constructor  =
  { rc_label: label
  ; rc_flds: 't field list
  ; rc_repr: constr_repr
  }

and ('s, 't) inlined_constructor  =
  { ic_label: label
  ; ic_flds: 't record_field list
  ; ic_repr: constr_repr
  }

and 's constructor =
  | Constant : 's constant_constructor -> 's constructor
  | Regular : ('s, 't) regular_constructor -> 's constructor
  | Inlined : ('s, 't) inlined_constructor -> 's constructor

and 's sum =
  { cstrs : 's constructor list }

and ('s, 't) arrow =
  { arg_label : string option
  ; arg_t: 's t
  ; res_t: 't t
  }

and 's method_ =
  | Method: string * ('s, 't) element -> 's method_

and 's object_ =
  { methods : 's method_ list }

(* internal Helpers *)
let cast_ttype: stype -> 'a ttype = Obj.magic
let cast_xtype: type a b. a xtype -> b xtype = Obj.magic
module StepMeta = Path.Internal [@@ocaml.warning "-3"]

(* Memoize xtype in stype node *)
type memoized_type_prop += Xtype of Obj.t xtype

let rec search a n i =
  if i = n then None
  else match a.(i) with
  | Xtype r -> Some r
  | _ -> search a n (i + 1)

let is_memoized (node : node) : Obj.t xtype option =
  search node.rec_memoized (Array.length node.rec_memoized) 0

let memoize: type a. node -> a xtype -> a xtype = fun node xt ->
  let s = Xtype (Obj.magic xt) in
  let old = node.rec_memoized in
  let a = Array.make (Array.length old + 1) s in
  Array.blit old 0 a 1 (Array.length old);
  Internal.set_memoized node a;
  xt

let rec xtype_of_ttype : type a. a ttype -> a xtype = fun t ->
  (* CAUTION: This must be consistent with core/std.ml *)
  match stype_of_ttype t with
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
  | DT_abstract ("lazy_t", [t]) -> cast_xtype (Lazy (bundle t))
  | DT_arrow (l, t1, t2) ->
    let arg_label = match l with
      | "" -> None
      | s -> Some s
    in cast_xtype (Function {arg_label; arg_t = bundle t1; res_t = bundle t2})
  | DT_tuple l -> cast_xtype (Tuple {t_flds=fields l})
  | DT_node ({rec_descr = DT_record r; _} as node) ->
    begin match is_memoized node with
    | None -> memoize node (cast_xtype (Record (record r)))
    | Some xt -> cast_xtype xt
    end
  | DT_node ({rec_descr = DT_variant v; _} as node) ->
    begin match is_memoized node with
    | None -> memoize node (cast_xtype (Sum (sum v)))
    | Some xt -> cast_xtype xt
    end
  | DT_object methods -> cast_xtype (Object (object_ methods))
  | DT_prop (props, s) -> Prop(props, bundle s)
  | DT_abstract (name, l) -> Abstract (name, l)
  | DT_var _ -> assert false

and bundle : type a. stype -> a t = fun s ->
    let t = cast_ttype s
    in {t; xt = lazy (xtype_of_ttype t)}

and fields (fields : stype list) : 'a field list =
  List.mapi (fun i t ->
      Field { typ = bundle t
            ; nth = i }
    ) fields

and record_fields fields : 'a record_field list =
  List.mapi (fun nth (field_name, field_props, s) ->
      (field_name, field_props), Field { typ = bundle s ; nth }
    ) fields

and record (r: record_descr) : 'a record =
  let r_flds = record_fields r.record_fields in
  let r_repr : record_repr = match r.record_repr with
    | Record_unboxed -> Unboxed
    | Record_float -> Float
    | Record_regular -> Regular
    | Record_inline _ -> assert false
  in {r_flds; r_repr}

and sum (v: variant_descr) : 'a sum =
  let repr = match v.variant_repr with
    | Variant_unboxed -> fun _ -> Unboxed
    | Variant_regular -> fun tag -> Tag tag
  in
  let pincr i = let r = !i in incr i; r in (* post increment *)
  let cst_i, ncst_i = ref 0, ref 0 in
  let cstrs = List.map (fun (name, props, arg) ->
      let lbl = name, props in
      match arg with
      | C_tuple [] -> Constant {cc_label = lbl;  cc_nr = pincr cst_i}
      | C_tuple l ->
        let tag = pincr ncst_i in
        Regular { rc_label = lbl
                ; rc_flds = fields l
                ; rc_repr = repr tag }
      | C_inline (DT_node {rec_descr = DT_record r; _}) ->
        let tag = pincr ncst_i in
        Inlined { ic_label = lbl
                ; ic_flds = record_fields r.record_fields
                ; ic_repr = repr tag }
      | C_inline _ -> assert false
    ) v.variant_constrs
  in { cstrs }

and object_ methods : 'a object_ =
  let prepare (type a) nth (name, stype) =
    let typ = (bundle stype : a t) in
    Method (name, {typ; nth})
  in
  let methods = List.mapi prepare methods in { methods }

let t_of_ttype t = {t;xt = lazy (xtype_of_ttype t)}

(* builders *)

module Builder = struct

  type 'a t = { mk: 't. ('a, 't) element -> 't } [@@unboxed]

  let fields : type a. a field list -> a t -> a =
    fun flds b ->
      let arity = List.length flds in
      let o = Obj.new_block 0 arity in
      List.iteri (fun i -> function Field f ->
          Obj.set_field o i (Obj.repr (b.mk f))
        ) flds;
      Obj.magic o

  let tuple t = fields t.t_flds

  let record_fields : type a. a record_field list -> a t -> a =
    fun flds b ->
      let flds = List.map (fun (_, f) -> f) flds in
      fields flds b

  let record : type a. a record -> a t -> a =
    fun {r_flds; r_repr} b -> match r_repr with
      | Regular -> record_fields r_flds b
      | Float ->
        let arity = List.length r_flds in
        let o = Obj.new_block Obj.double_array_tag arity in
        List.iteri (fun i -> function _, Field f ->
            Obj.set_double_field o i (Obj.magic (b.mk f))
          ) r_flds ;
        Obj.magic o
      | Unboxed -> begin match r_flds with
          | [_, Field f] -> Obj.magic (b.mk f)
          | _ -> assert false
        end

  let constant_constructor : type a. a constant_constructor -> a =
    fun {cc_nr;_} -> Obj.magic cc_nr

  let regular_constructor : type a b. (a, b) regular_constructor -> b t -> a =
    fun {rc_flds; rc_repr;_} b ->
      match rc_repr, rc_flds with
      | Tag tag, _ ->
        let o = Obj.repr (fields rc_flds b) in
        Obj.set_tag o tag; Obj.magic o
      | Unboxed, [Field f] -> Obj.magic (b.mk f)
      | Unboxed, _ -> assert false

  let inlined_constructor : type a b. (a, b) inlined_constructor -> b t -> a =
    fun {ic_flds; ic_repr; _} b ->
      match ic_repr, ic_flds with
      | Tag tag, _ ->
        let o = Obj.repr (record_fields ic_flds b) in
        Obj.set_tag o tag; Obj.magic o
      | Unboxed, [_, Field f] -> Obj.magic (b.mk f)
      | Unboxed, _ -> assert false

  type generic = { mk: 's 't. ('s, 't) element -> 't } [@@unboxed]

  let constructor : type a. a constructor -> generic -> a =
    fun c b ->
      match c with
      | Constant c -> constant_constructor c
      | Regular c -> regular_constructor c { mk = b.mk }
      | Inlined c -> inlined_constructor c { mk = b.mk }
end

module Make = struct

  type 'a t = Obj.t option array
  exception Missing_field of string

  let set: 'a t -> ('a, 'b) element  -> 'b -> unit =
    fun arr {nth;_} x -> Array.set arr nth (Some (Obj.repr x))

  (* Reuse Builder *)

  let builder : ('a t -> unit) -> int -> 'a Builder.t =
    fun init n ->
      let arr = Array.make n None in
      let mk {nth; _} = match arr.(nth) with
        | None -> raise (Missing_field (string_of_int nth))
        | Some o -> Obj.magic o
      in init arr; {mk}

  let tuple: 'a tuple -> ('a t -> unit) -> 'a =
    fun {t_flds} f ->
      Builder.fields t_flds (builder f (List.length t_flds))

  let record: 'a record -> ('a t -> unit) -> 'a =
    fun r f ->
      Builder.record r (builder f (List.length r.r_flds))

  let regular_constructor: type a b.
    (a, b) regular_constructor -> (b t -> unit) -> a =
    fun c f ->
      Builder.regular_constructor c (builder f (List.length c.rc_flds))

  let inlined_constructor: type a b.
    (a, b) inlined_constructor -> (b t -> unit) -> a =
    fun c f ->
      Builder.inlined_constructor c (builder f (List.length c.ic_flds))
end


(* property handling *)

let get_first_props_xtype xt =
  let rec loop accu = function
    | Prop (l, {xt=lazy xt;_}) ->
        loop (l :: accu) xt
    | _ ->
        List.concat (List.rev accu)
  in
  loop [] xt

let rec remove_first_props_xtype : type t. t xtype -> t xtype = function
  | Prop (_, {xt=lazy xt;_}) -> remove_first_props_xtype xt
  | xt -> xt

(* fast lookup for named elements *)

module Lookup = struct

  let finder get_name (lst: 'a list) : (string -> 'a option)=
    let tbl = lazy (
      Ext.String.Tbl.prepare (List.map get_name lst))
    in
    let arr = Array.of_list lst in
    fun n ->
      let i = Ext.String.Tbl.lookup (Lazy.force tbl) n in
      if i < 0 then None else Some arr.(i)

  let record_field r =
    let f = function f -> fst (fst f) in
    finder f r.r_flds

  let constructor_field c =
    let f = function f -> fst (fst f) in
    finder f c.ic_flds

  let constructor s =
    let f = function
      | Constant {cc_label=(name, _); _}
      | Regular {rc_label=(name, _); _}
      | Inlined {ic_label=(name, _); _} -> name
    in finder f s.cstrs

  let method_ o =
    let f = function Method (name, _) -> name in
    finder f o.methods
end

let constructor_by_value : type a. a sum -> a -> a constructor =
  fun sum ->
    let cst, ncst = ref [], ref [] in
    List.iteri (fun i -> function
        | Constant _ -> cst := i :: !cst
        | Regular _
        | Inlined _ -> ncst := i :: !ncst
      ) sum.cstrs;
    let constructors = Array.of_list sum.cstrs in
    let cst = Ext.Array.of_list_rev !cst in
    let ncst = Ext.Array.of_list_rev !ncst in
    fun x ->
      let i =
        let x = Obj.repr x in
        if Obj.is_int x then Array.get cst (Obj.magic x)
        else Array.get ncst (Obj.tag x)
      in constructors.(i)

let call_method: 'a object_ -> ('a, 'b) element -> 'a -> 'b =
  fun o e ->
    let Method (name,_) = List.nth o.methods (e.nth) in
    let label = CamlinternalOO.public_method_label name in
    fun x -> Obj.magic (CamlinternalOO.send (Obj.magic x) label)

(* paths *)

module Step = struct
  let cast : (Obj.t, Obj.t) Path.lens -> ('a, 'b) Path.lens = Obj.magic
  let check tag o = Obj.is_block o && Obj.tag o = tag

  let tuple: 'a tuple -> ('a, 'b) element-> ('a,'b) Path.step =
    fun tup f ->
      let arity = List.length tup.t_flds in
      let nth = f.nth in
      let get = (fun o -> Some (Obj.field o nth)) in
      let set =
        (fun o v ->
           let o = Obj.dup o in
           Obj.set_field o nth (Obj.repr v); Some o)
      in
      cast { get ; set }, StepMeta.tuple ~nth ~arity

  let record: 'a record -> ('a, 'b) element -> ('a,'b) Path.step =
    fun r f ->
      let i = f.nth in
      let (field_name, _),_ = List.nth r.r_flds i in
      let get, set = match r.r_repr with
        | Regular ->
          (fun r -> Some (Obj.field r i)),
          (fun r v ->
             let r = Obj.dup r in
             Obj.set_field r i (Obj.repr v); Some r)
        | Float ->
          ( fun r -> Some (Obj.repr (Obj.double_field r i))),
          ( fun r v ->
              let r = Obj.dup r in
              Obj.set_double_field r i (Obj.magic v); Some r)
        | Unboxed ->
          (fun r -> Some (Obj.repr r)),
          (fun _r v -> Some (Obj.repr v))
      in
      cast {get; set}, StepMeta.field ~field_name

  let regular_constructor:
    ('a, 'b) regular_constructor -> ('b,'c) element -> ('a,'c) Path.step =
    fun c f ->
      let arity = List.length c.rc_flds in
      let nth = f.nth in
      let get, set = match c.rc_repr with
      | Unboxed ->
        (fun o -> Some o),
        (fun _o v -> Some v)
      | Tag tag ->
        (fun o -> if check tag o then Some (Obj.field o nth) else None),
        (fun o v -> if check tag o then
            let o = Obj.dup o in
            Obj.set_field o nth (Obj.repr v); Some o
          else None)
      in
      let name = fst c.rc_label in
      cast { get ; set }, StepMeta.constructor_regular ~name ~nth ~arity

  let inlined_constructor:
    ('a, 'b) inlined_constructor -> ('b,'c) element -> ('a,'c) Path.step =
    fun c f ->
      let i = f.nth in
      let (field_name, _),_ = List.nth c.ic_flds i in
      let get, set = match c.ic_repr with
        | Tag tag ->
          (fun r -> if check tag r then
              Some (Obj.field r i) else None),
          (fun r v -> if check tag r then
              let r = Obj.dup r in
              Obj.set_field r i (Obj.repr v); Some r
            else None)
        | Unboxed ->
          (fun r -> Some (Obj.repr r)),
          (fun _r v -> Some (Obj.repr v))
      in
      let name = fst c.ic_label in
      cast {get; set},
      StepMeta.constructor_inline ~name ~field_name
end

let option_step_some : type a. a ttype -> (a option, a) Path.step =
  (* ttype helps to avoid Obj.magic *)
  fun _t ->
    let get x = x
    and set x v = match x with
      | None -> None
      | Some _ -> Some (Some v)
    in Path.{ get; set} ,
       StepMeta.constructor_regular ~name:"Some" ~nth:0 ~arity:1

let rec all_paths: type a b. a ttype -> b ttype -> (a, b) Path.t list =
  fun root target ->
    match ttypes_equality root target with
    | Some TypEq.Eq -> [Path.[]]
    | None ->
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
      | Option {t;_} ->
        let paths = all_paths t target in
        List.map Path.(fun p -> option_step_some t :: p) paths
      | Tuple t -> tuple ~target t
      | Record r -> record ~target r
      | Sum sum ->
        List.map
          (function
             | Constant _ -> []
             | Regular c -> regular_constructor ~target c
             | Inlined c -> inlined_constructor ~target c
          ) sum.cstrs
        |> List.concat
      | Prop (_, {t;_}) -> all_paths t target
      | Object _ -> []
      | List _ -> []
      | Array _ -> []
      | Function _ -> []
      | Lazy _ -> []
      | Abstract _ -> []

and tuple : type a b. target: b ttype -> a tuple -> (a, b) Path.t list =
  fun ~target tup ->
    List.map
      (function (Field f) ->
         let paths = all_paths f.typ.t target in
         List.map Path.(fun p -> Step.tuple tup f :: p) paths
      ) tup.t_flds
    |> List.concat

and record :
  type a b. target: b ttype -> a record -> (a, b) Path.t list =
  fun ~target r ->
    List.map
      (function (_, Field f) ->
         let paths = all_paths f.typ.t target in
         List.map Path.(fun p -> Step.record r f :: p) paths
      ) r.r_flds
    |> List.concat

and regular_constructor : type a b c.
  target: c ttype -> (a, b) regular_constructor -> (a, c) Path.t list =
  fun ~target c ->
    List.map
      (function (Field f) ->
         let paths = all_paths f.typ.t target in
         List.map Path.(fun p -> Step.regular_constructor c f :: p) paths
      ) c.rc_flds
    |> List.concat

and inlined_constructor : type a b c.
  target: c ttype -> (a, b) inlined_constructor -> (a, c) Path.t list =
  fun ~target c ->
    List.map
      (function (_, Field f) ->
         let paths = all_paths f.typ.t target in
         List.map Path.(fun p -> Step.inlined_constructor c f :: p) paths
      ) c.ic_flds
    |> List.concat

(* Project ttype along path *)

let assert_some = function
  | Some x -> x
  | None -> assert false

let cast : type a b. a t -> b ttype = fun t -> Obj.magic t.t

let rec project_path : type a b. a ttype -> (a,b) Path.t -> b ttype =
  fun t -> function
    | [] -> t
    | (_, meta) :: tl ->
      let t = match meta, xtype_of_ttype t with
        | Field {field_name}, Record r ->
          (Lookup.record_field r field_name |> assert_some
           |> function _, Field f -> cast f.typ)
        | Tuple {nth; _}, Tuple t ->
          (List.nth t.t_flds nth |> function Field f -> cast f.typ)
        | List _, List t -> cast t
        | Array _, Array t -> cast t
        | Constructor {name; arg}, Sum s ->
          ( match arg, (Lookup.constructor s name |> assert_some) with
            | Regular {nth;_}, Regular c ->
              (List.nth c.rc_flds nth |> function Field f -> cast f.typ)
            | Inline {field_name}, Inlined c ->
              (Lookup.constructor_field c field_name |> assert_some
               |> function _, Field f -> cast f.typ)
            | Regular _, _
            | Inline _, _ -> assert false
          )
        | _ -> assert false
      in project_path t tl

(* type matching *)

module type TYPE_0 = sig
  type t
  val t: t ttype
end

module type TYPE_1 = sig
  type 'a t
  val t: 'a ttype -> 'a t ttype
end

module type TYPE_2 = sig
  type ('a, 'b) t
  val t: 'a ttype -> 'b ttype -> ('a, 'b) t ttype
end

module type MATCHER_0 = sig
  include TYPE_0
  type _ is_t = Is: ('a, t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a ttype -> 'a is_t option
  val is_abstract: string option
end

module type MATCHER_1 = sig
  include TYPE_1
  type _ is_t = Is: 'b ttype * ('a, 'b t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a ttype -> 'a is_t option
  val is_abstract: string option
end

module type MATCHER_2 = sig
  include TYPE_2
  type _ is_t = Is: 'b ttype * 'c ttype * ('a, ('b, 'c) t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a ttype -> 'a is_t option
  val is_abstract: string option
end

let get_abstract_name (s: stype) =
  match remove_first_props s with
  | DT_abstract (name, _) -> Some name
  | _ -> None

exception Not_unifiable

let rec unifier_list_iter2 f l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | [], _
  | _, [] -> raise Not_unifiable
  | h1 :: t1 , h2 :: t2 -> f h1 h2 ; unifier_list_iter2 f t1 t2

let variant_constrs_iter2 (f: stype -> stype -> unit)
    (name1, props1, vargs1)
    (name2, props2, vargs2) =
  if name1 <> name2 then raise Not_unifiable;
  if props1 <> props2 then raise Not_unifiable;
  match vargs1, vargs2 with
  | C_inline s1, C_inline s2 -> f s1 s2
  | C_tuple l1, C_tuple l2 -> unifier_list_iter2 f l1 l2
  | C_inline _, _
  | C_tuple _, _ -> raise Not_unifiable

(* iterate over all stypes in a node *)
let node_iter2 (f: stype -> stype -> unit)
    ({rec_descr = descr1; rec_uid = uid1; rec_name = name1; rec_args = args1; rec_has_var = _; _}: node)
    ({rec_descr = descr2; rec_uid = uid2; rec_name = name2; rec_args = args2; _}: node) =
  if uid1 <> uid2 || name1 <> name2 then raise Not_unifiable;
  unifier_list_iter2 f args1 args2;
  match descr1, descr2 with
  | DT_variant {variant_constrs = c1; variant_repr = r1},
    DT_variant {variant_constrs = c2; variant_repr = r2} when r1 = r2 ->
    unifier_list_iter2 (variant_constrs_iter2 f) c1 c2
  | DT_record {record_fields = l1; record_repr = r1},
    DT_record {record_fields = l2; record_repr = r2} when r1 = r2 ->
    unifier_list_iter2 (
      fun (name1, props1, s1) (name2, props2, s2) ->
        if name1 <> name2 then raise Not_unifiable;
        if props1 <> props2 then raise Not_unifiable;
        f s1 s2
    ) l1 l2
  | DT_record _, _
  | DT_variant _, _ -> raise Not_unifiable

let unifier ~(modulo_props : bool) ~(subs : stype option array) (s1 : stype) (s2 : stype) =
  let l = Array.length subs in
  let set k s =
    if k < 0 || k >= l then raise (Invalid_argument "unifier: variable index out of bounds") ;
    match subs.(k) with
    | None -> subs.(k) <- Some s
    | Some s' ->
      if s <> s' then raise Not_unifiable
  in
  let rec unifier s1 s2 =
    match s1, s2 with
    | DT_var _, DT_var _ -> raise (Invalid_argument "unifier: received type variable on the right")
    | DT_var k, s2 -> set k s2
    | DT_int, DT_int
    | DT_float, DT_float
    | DT_string, DT_string -> ()
    | DT_option s1, DT_option s2
    | DT_list s1, DT_list s2
    | DT_array s1, DT_array s2 -> unifier s1 s2
    | DT_tuple l1, DT_tuple l2 ->
      unifier_list_iter2 unifier l1 l2
    | DT_node n1, DT_node n2 -> node_iter2 unifier n1 n2
    | DT_arrow (n1, s1, s1'), DT_arrow (n2, s2, s2') ->
      if n1 <> n2 then raise Not_unifiable;
      unifier s1  s2 ;
      unifier s1' s2'
    | DT_object l1, DT_object l2 ->
      unifier_list_iter2 (fun (n1,s1) (n2,s2) ->
          if n1 <> n2 then raise Not_unifiable;
          unifier s1 s2
        ) l1 l2
    | DT_abstract (n1, l1), DT_abstract (n2, l2) ->
      if n1 <> n2 then raise Not_unifiable;
      unifier_list_iter2 unifier l1 l2
    | DT_prop (p1, t1), DT_prop (p2, t2) when p1 = p2 -> unifier t1 t2
    | DT_prop (_, t1), t2 when modulo_props -> unifier t1 t2
    | t1, DT_prop (_, t2) when modulo_props -> unifier t1 t2
    | DT_prop _, _
    | DT_int, _
    | DT_float, _
    | DT_string, _
    (* | DT_date, _ *)
    | DT_option _, _
    | DT_list _, _
    | DT_array _, _
    | DT_tuple _, _
    | DT_node _, _
    | DT_arrow _, _
    | DT_object _, _
    | DT_abstract _, _ -> raise Not_unifiable
  in
  unifier s1 s2

module Matcher_0 (T : TYPE_0) = struct
  include T

  type _ is_t = Is: ('a, t) TypEq.t -> 'a is_t

  let s = stype_of_ttype T.t
  let is_abstract = get_abstract_name s

  let key =
    let () =
      (* Check for free trype variables and fail early *)
      if Internal.has_var s
      then failwith (Format.asprintf "Xtypes: invalid MATCHER_0 witness: %a" print_stype s)
    in
    s

  let is_t ?(modulo_props=false) (t : 'a ttype) : 'a is_t option =
    try
      let subs = Array.make 0 None in
      unifier ~modulo_props ~subs key (stype_of_ttype t);
      Some (Is (Obj.magic TypEq.refl))
    with
      Not_unifiable -> None

end

module Matcher_1 (T : TYPE_1) = struct
  include T

  type _ is_t = Is: 'b ttype * ('a, 'b T.t) TypEq.t -> 'a is_t

  let s = T.t Std.unit_t |> stype_of_ttype
  let is_abstract = get_abstract_name s

  let key =
    let () =
      (* Check for free trype variables and fail early *)
      if Internal.has_var s
      then failwith (Format.asprintf "Xtypes: invalid MATCHER_1 witness: %a" print_stype s)
    in
    stype_of_ttype (T.t (cast_ttype (DT_var 0)))

  let is_t ?(modulo_props=false) (type s) (t : s ttype) : s is_t option =
    try
      let subs = Array.make 1 None in
      unifier ~modulo_props ~subs key (stype_of_ttype t);
      match subs with
      | [|Some s|] -> Some (Is (cast_ttype s, Obj.magic (TypEq.refl)))
      | _ -> None
    with
      Not_unifiable -> None
end

module Matcher_2 (T : TYPE_2) = struct
  include T

  type _ is_t = Is: 'aa ttype * 'bb ttype * ('a, ('aa,'bb) t) TypEq.t -> 'a is_t

  let s = T.t Std.unit_t Std.unit_t |> stype_of_ttype
  let is_abstract = get_abstract_name s

  let key =
    let () =
      (* Check for free trype variables and fail early *)
      if Internal.has_var s
      then failwith (Format.asprintf "Xtypes: invalid MATCHER_2 witness: %a" print_stype s)
    in
    stype_of_ttype (T.t (cast_ttype (DT_var 0)) (cast_ttype (DT_var 1)))

  let is_t ?(modulo_props=false) (type s) (t : s ttype) : s is_t option =
    try
      let subs = Array.make 2 None in
      unifier ~modulo_props ~subs key (stype_of_ttype t);
      match subs with
      | [|Some s1; Some s2|] -> Some (Is (cast_ttype s1, cast_ttype s2, Obj.magic (TypEq.refl)))
      | _ -> None
    with
      Not_unifiable -> None
end

