type record_repr = Regular | Float | Unboxed
type constr_repr = Tag of int | Unboxed

type 'a t =
  { t: 'a Ttype.t
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
  | List: 'b t -> 'b list xtype
  | Option: 'b t -> 'b option xtype
  | Array: 'b t -> 'b array xtype
  | Lazy: 'b t -> 'b Lazy.t xtype
  | Tuple: 'a tuple -> 'a xtype
  | Record: 'a record -> 'a xtype
  | Sum: 'a sum -> 'a xtype
  | Function: ('b,'c) arrow -> ('b -> 'c) xtype
  | Object: 'a object_ -> 'a xtype
  | Prop: (Stype.properties * 'a t) -> 'a xtype
  | Abstract: (string * Stype.t list) -> 'a xtype

and ('s,'t) element =
  { typ: 't t
  ; nth: int
  }

and 's field =
  | Field: ('s, 't) element -> 's field

and 's tuple =
  { t_flds : 's field list }

and label = string * Stype.properties

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
let cast_ttype: Stype.t -> 'a Ttype.t = Obj.magic
let cast_xtype: type a b. a xtype -> b xtype = Obj.magic
module StepMeta = Path.Internal [@@ocaml.warning "-3"]

(* Memoize xtype in stype node *)
module M = struct
  open Stype
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
end

let rec xtype_of_ttype : type a. a Ttype.t -> a xtype = fun t ->
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
    let arg_label = match l with
      | "" -> None
      | s -> Some s
    in cast_xtype (Function {arg_label; arg_t = bundle t1; res_t = bundle t2})
  | DT_tuple l -> cast_xtype (Tuple {t_flds=fields l})
  | DT_node ({rec_descr = DT_record r; _} as node) ->
    begin match M.is_memoized node with
    | None -> M.memoize node (cast_xtype (Record (record r)))
    | Some xt -> cast_xtype xt
    end
  | DT_node ({rec_descr = DT_variant v; _} as node) ->
    begin match M.is_memoized node with
    | None -> M.memoize node (cast_xtype (Sum (sum v)))
    | Some xt -> cast_xtype xt
    end
  | DT_object methods -> cast_xtype (Object (object_ methods))
  | DT_prop (props, s) -> Prop(props, bundle s)
  | DT_abstract (name, l) -> Abstract (name, l)
  | DT_var _ -> assert false

and bundle : type a. Stype.t -> a t = fun s ->
    let t = cast_ttype s
    in {t; xt = lazy (xtype_of_ttype t)}

and fields (fields : Stype.t list) : 'a field list =
  List.mapi (fun i t ->
      Field { typ = bundle t
            ; nth = i }
    ) fields

and record_fields fields : 'a record_field list =
  List.mapi (fun nth (field_name, field_props, s) ->
      (field_name, field_props), Field { typ = bundle s ; nth }
    ) fields

and record (r: Stype.record_descr) : 'a record =
  let r_flds = record_fields r.record_fields in
  let r_repr : record_repr = match r.record_repr with
    | Record_unboxed -> Unboxed
    | Record_float -> Float
    | Record_regular -> Regular
    | Record_inline _ -> assert false
  in {r_flds; r_repr}

and sum (v: Stype.variant_descr) : 'a sum =
  let repr = match v.variant_repr with
    | Variant_unboxed -> fun _ -> Unboxed
    | Variant_regular -> fun tag -> Tag tag
  in
  let pincr i = let r = !i in incr i; r in (* post increment *)
  let cst_i, ncst_i = ref 0, ref 0 in
  let cstrs = List.map (fun (name, props, arg) ->
      let lbl = name, props in
      let open Stype in
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

let of_ttype t = {t; xt = lazy (xtype_of_ttype t)}

(* builders *)

module Builder = struct

  type 'a t = { mk: 't. ('a, 't) element -> 't } [@@unboxed]
  type 's t' = { mk: 't. label -> ('s, 't) element -> 't } [@@unboxed]

  let fields : type a. a field list -> a t -> a =
    fun flds b ->
      let arity = List.length flds in
      let o = Obj.new_block 0 arity in
      List.iteri (fun i -> function Field f ->
          Obj.set_field o i (Obj.repr (b.mk f))
        ) flds;
      Obj.magic o

  let record_fields : type a. a record_field list -> a t' -> a =
    fun flds b ->
      let arity = List.length flds in
      let o = Obj.new_block 0 arity in
      List.iteri (fun i -> function l, Field f ->
          Obj.set_field o i (Obj.repr (b.mk l f))
        ) flds;
      Obj.magic o

  let tuple t = fields t.t_flds

  let record : type a. a record -> a t' -> a =
    fun {r_flds; r_repr} b -> match r_repr with
      | Regular -> record_fields r_flds b
      | Float ->
        let arity = List.length r_flds in
        let o = Obj.new_block Obj.double_array_tag arity in
        List.iteri (fun i -> function l, Field f ->
            Obj.set_double_field o i (Obj.magic (b.mk l f))
          ) r_flds ;
        Obj.magic o
      | Unboxed -> begin match r_flds with
          | [l, Field f] -> Obj.magic (b.mk l f)
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

  let inlined_constructor : type a b. (a, b) inlined_constructor -> b t' -> a =
    fun {ic_flds; ic_repr; _} b ->
      match ic_repr, ic_flds with
      | Tag tag, _ ->
        let o = Obj.repr (record_fields ic_flds b) in
        Obj.set_tag o tag; Obj.magic o
      | Unboxed, [l, Field f] -> Obj.magic (b.mk l f)
      | Unboxed, _ -> assert false

  type generic = { mk: 's 't. ('s, 't) element -> 't } [@@unboxed]

  let constructor : type a. a constructor -> generic -> a =
    fun c b ->
      match c with
      | Constant c -> constant_constructor c
      | Regular c -> regular_constructor c { mk = b.mk }
      | Inlined c -> inlined_constructor c { mk = fun _ -> b.mk }
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

  let named_builder : ('a t -> unit) -> int -> 'a Builder.t' =
    fun init n ->
      let arr = Array.make n None in
      let mk _ {nth; _} = match arr.(nth) with
        | None -> raise (Missing_field (string_of_int nth))
        | Some o -> Obj.magic o
      in init arr; {mk}

  let tuple: 'a tuple -> ('a t -> unit) -> 'a =
    fun {t_flds} f ->
      Builder.fields t_flds (builder f (List.length t_flds))

  let record: 'a record -> ('a t -> unit) -> 'a =
    fun r f ->
      Builder.record r (named_builder f (List.length r.r_flds))

  let regular_constructor: type a b.
    (a, b) regular_constructor -> (b t -> unit) -> a =
    fun c f ->
      Builder.regular_constructor c (builder f (List.length c.rc_flds))

  let inlined_constructor: type a b.
    (a, b) inlined_constructor -> (b t -> unit) -> a =
    fun c f ->
      Builder.inlined_constructor c (named_builder f (List.length c.ic_flds))
end

module Fields = struct
  let check tag o = Obj.is_block o && Obj.tag o = tag

  let cast: (Obj.t -> Obj.t) -> 'a -> 'b = Obj.magic

  let tuple : type a b. a tuple -> (a, b) element -> a -> b =
    fun _ el -> cast (fun o -> Obj.field o el.nth)

  let record : type a b. a record -> (a, b) element -> a -> b =
    fun r el -> cast (match r.r_repr with
      | Regular -> fun o -> Obj.field o el.nth
      | Float -> fun o -> Obj.repr (Obj.double_field o el.nth)
      | Unboxed -> fun o -> Obj.repr o)

  let cast: (Obj.t -> Obj.t option) -> 'a -> 'b option = Obj.magic
  let checked tag i o =
      if check tag o then Some (Obj.field o i) else None

  let regular_constructor : type a b c.
    (a, b) regular_constructor -> (b,c) element -> a -> c option =
    fun c el -> cast (match c.rc_repr with
      | Tag tag -> checked tag el.nth
      | Unboxed -> fun o -> Some (Obj.repr o))

  let inlined_constructor : type a b c.
    (a, b) inlined_constructor -> (b,c) element -> a -> c option =
    fun c el -> cast (match c.ic_repr with
      | Tag tag -> checked tag el.nth
      | Unboxed -> fun o -> Some (Obj.repr o))

  let map_tuple tup f x =
    let mapf (Field e) = f (Ttype.Dyn (e.typ.t, tuple tup e x)) in
    List.map mapf tup.t_flds

  let map_record r f x =
    let mapf ((name,_), Field e) = f ~name (Ttype.Dyn (e.typ.t, record r e x)) in
    List.map mapf r.r_flds

  let map_regular c f x =
    let mapf (Field e) =
      f (Ttype.Dyn (e.typ.t, Ext.Option.value_exn (regular_constructor c e x)))
    in
    List.map mapf c.rc_flds

  let map_inlined c f x =
    let mapf ((name,_), Field e) =
      f ~name (Ttype.Dyn (e.typ.t,
                          Ext.Option.value_exn (inlined_constructor c e x)))
    in
    List.map mapf c.ic_flds
end

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

  (* TODO: This could use the above Read module *)

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

let option_step_some : type a. a Ttype.t -> (a option, a) Path.step =
  (* ttype helps to avoid Obj.magic *)
  fun _t ->
    let get x = x
    and set x v = match x with
      | None -> None
      | Some _ -> Some (Some v)
    in Path.{ get; set} ,
       StepMeta.constructor_regular ~name:"Some" ~nth:0 ~arity:1

let rec all_paths: type a b. a Ttype.t -> b Ttype.t -> (a, b) Path.t list =
  fun root target ->
    match Ttype.equality root target with
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

and tuple : type a b. target: b Ttype.t -> a tuple -> (a, b) Path.t list =
  fun ~target tup ->
    List.map
      (function (Field f) ->
         let paths = all_paths f.typ.t target in
         List.map Path.(fun p -> Step.tuple tup f :: p) paths
      ) tup.t_flds
    |> List.concat

and record :
  type a b. target: b Ttype.t -> a record -> (a, b) Path.t list =
  fun ~target r ->
    List.map
      (function (_, Field f) ->
         let paths = all_paths f.typ.t target in
         List.map Path.(fun p -> Step.record r f :: p) paths
      ) r.r_flds
    |> List.concat

and regular_constructor : type a b c.
  target: c Ttype.t -> (a, b) regular_constructor -> (a, c) Path.t list =
  fun ~target c ->
    List.map
      (function (Field f) ->
         let paths = all_paths f.typ.t target in
         List.map Path.(fun p -> Step.regular_constructor c f :: p) paths
      ) c.rc_flds
    |> List.concat

and inlined_constructor : type a b c.
  target: c Ttype.t -> (a, b) inlined_constructor -> (a, c) Path.t list =
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

let cast : type a b. a t -> b Ttype.t = fun t -> Obj.magic t.t

let rec project_path : type a b. a Ttype.t -> (a,b) Path.t -> b Ttype.t =
  fun t -> let open Path in function
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

open Unify
module type T0 = T0
module type T1 = T1
module type T2 = T2

module type MATCH0 = sig
  include T0
  type _ is_t = Is: ('a, t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a Ttype.t -> 'a is_t option
  val is_abstract: string option
  (** If [t] is an abstract type, [is_abstract t] provides its identifier.*)
end

module type MATCH1 = sig
  include T1
  type _ is_t = Is: 'b Ttype.t * ('a, 'b t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a Ttype.t -> 'a is_t option
  val is_abstract: string option
  (** If [t] is an abstract type, [is_abstract t] provides its identifier.*)
end

module type MATCH2 = sig
  include T2
  type _ is_t =
      Is: 'b Ttype.t * 'c Ttype.t * ('a, ('b, 'c) t) TypEq.t -> 'a is_t
  val is_t: ?modulo_props : bool -> 'a Ttype.t -> 'a is_t option
  val is_abstract: string option
  (** If [t] is an abstract type, [is_abstract t] provides its identifier.*)
end

let get_abstract_name (s: Stype.t) =
  match Stype.remove_outer_props s with
  | DT_abstract (name, _) -> Some name
  | _ -> None

module Match0 (T : T0) = struct
  include T

  type _ is_t = Is: ('a, t) TypEq.t -> 'a is_t

  let s = Ttype.to_stype T.t
  let is_abstract = get_abstract_name s

  let is_t (type t') ?(modulo_props=false) (t': t' Ttype.t) : t' is_t option =
    ignore modulo_props;
    try
      let module U = U0 (T) (struct type t = t' let t = t' end) in
      Some (Is (TypEq.sym U.eq))
    with
      Not_unifiable -> None
end

module Match1 (T : T1) = struct
  include T

  type _ is_t = Is: 'b Ttype.t * ('a, 'b T.t) TypEq.t -> 'a is_t

  let s = T.t Std.unit_t |> Ttype.to_stype
  let is_abstract = get_abstract_name s

  let is_t (type t') ?(modulo_props=false) (t': t' Ttype.t) : t' is_t option =
    ignore modulo_props;
    try
      let module U = U1 (T) (struct type t = t' let t = t' end) in
      Some (Is (U.a_t, TypEq.sym U.eq))
    with
      Not_unifiable -> None
end

module Match2 (T : T2) = struct
  include T

  type _ is_t =
      Is: 'aa Ttype.t * 'bb Ttype.t * ('a, ('aa,'bb) t) TypEq.t -> 'a is_t

  let s = T.t Std.unit_t Std.unit_t |> Ttype.to_stype
  let is_abstract = get_abstract_name s

  let is_t (type t') ?(modulo_props=false) (t': t' Ttype.t) : t' is_t option =
    ignore modulo_props;
    try
      let module U = U2 (T) (struct type t = t' let t = t' end) in
      Some (Is (U.a_t, U.b_t, TypEq.sym U.eq))
    with
      Not_unifiable -> None
end

