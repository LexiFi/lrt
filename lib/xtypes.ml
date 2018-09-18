open Dynt_core
open Dynt_core.Ttype
open Dynt_core.Stype

type record_repr = Regular | Float | Unboxed
type constr_repr = Tag of int | Unboxed

type 'a t = 'a ttype * 'a xtype Lazy.t
(** The construction of xtypes is expensive and should not happen recursively.*)

and 'a xtype
  = Unit: unit xtype
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

and ('s,'t) field =
  { t: 't t
  ; nth: int
  }

and 's has_field = Field: ('s, 't) field -> 's has_field

and 's tuple = 's has_field array

and ('s, 't) named_field =
  { field: ('s, 't) field
  ; field_name: string
  ; field_props: stype_properties
  }

and 's has_named_field = NamedField: ('s, 't) named_field -> 's has_named_field

and 's named_tuple =
  { fields: 's has_named_field array
  ; find_field: string -> 's has_named_field option
  }

and 's record = 's named_tuple * record_repr

and ('s, _) constr_kind =
  | Constant : int -> ('s, [> `Constant]) constr_kind
  | Regular : 's tuple * constr_repr -> ('s, [> `Regular]) constr_kind
  | Inlined : 's named_tuple * constr_repr -> ('s, [> `Inlined]) constr_kind

and ('s, 'kind) constructor =
  { constr_name: string
  ; constr_props: stype_properties
  ; kind: ('s, 'kind) constr_kind
  }

and 's has_constructor = ('s, [`Constant|`Regular|`Inlined]) constructor

and 's sum =
  { constructors: 's has_constructor array
  ; find_constructor: string -> 's has_constructor option
  ; constructor: 's -> 's has_constructor
  }

and ('s, 't) arrow =
  { label : string option
  ; from_t: 's t
  ; to_t: 't t
  }

and ('s, 't) method_ =
  { method_name: string
  ; method_type: 't t
  ; call: 's -> 't
  }

and 's has_method = Method: ('s, 't) method_ -> 's has_method

and 's object_ =
  { methods : 's has_method array
  ; find_method : string -> 's has_method option
  }

type 's constant_constructor = ('s, [`Constant]) constructor
type 's regular_constructor = ('s, [`Regular]) constructor
type 's inlined_constructor = ('s, [`Inlined]) constructor

(* internal Helpers *)
let cast_ttype: stype -> 'a ttype = Obj.magic
let cast_xtype: type a b. a xtype -> b xtype = Obj.magic
module StepMeta = Path.Internal [@@ocaml.warning "-3"]

(* Find fields / methods / constructors by name *)
let finder get_name (arr: 'a array) : (string -> 'a option)=
  let tbl = lazy (
    Ext.String.Tbl.prepare (Ext.Array.map_to_list get_name arr))
  in
  fun n ->
    let i = Ext.String.Tbl.lookup (Lazy.force tbl) n in
    if i < 0 then None else Some arr.(i)

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
    let label = match l with
      | "" -> None
      | s -> Some s
    in cast_xtype (Function {label; from_t = bundle t1; to_t = bundle t2})
  | DT_tuple fields -> cast_xtype (Tuple (tuple fields))
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
    in (t, lazy (xtype_of_ttype t))

and tuple fields : 'a has_field array =
  let fields = List.mapi (fun i t ->
      Field { t = bundle t
            ; nth = i }
    ) fields
  in Array.of_list fields

and named_tuple record : 'a named_tuple =
  let fields = List.mapi (fun i (field_name, field_props, s) ->
      NamedField { field_name; field_props;
                   field = { t = bundle s
                           ; nth = i }}
    ) record.record_fields |> Array.of_list
  in
  let find_field = finder (fun (NamedField f) -> f.field_name) fields in
  { fields; find_field }

and record record : 'a record =
  let nt = named_tuple record in
  let repr : record_repr = match record.record_repr with
    | Record_unboxed -> Unboxed
    | Record_float -> Float
    | Record_regular -> Regular
    | Record_inline _ -> assert false
  in nt , repr

and sum variant : 'a sum =
  let repr = match variant.variant_repr with
    | Variant_unboxed -> fun _ -> Unboxed
    | Variant_regular -> fun tag -> Tag tag
  in
  let n = List.length variant.variant_constrs in
  let constructors = Array.make n
      { constr_name = ""
      ; constr_props = []
      ; kind = Constant 0}
  in
  let pincr i = let r = !i in incr i; r in
  let cst_i, ncst_i = ref 0, ref 0 in
  let cst, ncst = ref [], ref [] in
  List.iteri (fun i (name, constr_props, arg) ->
      let kind =
        match arg with
        | C_tuple [] -> cst := i :: !cst ; Constant (pincr cst_i)
        | C_tuple l ->
          ncst := i :: !ncst;
          let tag = pincr ncst_i in
          let repr = repr tag in
          Regular (tuple l, repr)
        | C_inline (DT_node {rec_descr = DT_record r; _}) ->
          ncst := i :: !ncst;
          let tag = pincr ncst_i in
          let repr = repr tag in
          Inlined (named_tuple r, repr)
        | C_inline _ -> assert false
      in
      constructors.(i) <- { kind; constr_props
                          ; constr_name = name}
    ) variant.variant_constrs;
  (* Lookup tables tag -> constructor index *)
  let cst = Ext.Array.of_list_rev !cst in
  let ncst = Ext.Array.of_list_rev !ncst in
  (* Constructor by value *)
  let constructor x : ('a, _) constructor =
    let i =
      let x = Obj.repr x in
      if Obj.is_int x then Array.get cst (Obj.magic x)
      else Array.get ncst (Obj.tag x)
    in constructors.(i)
  in
  let find_constructor = finder (fun c -> c.constr_name) constructors
  in { constructors; find_constructor; constructor }

and object_ methods : 'a object_ =
  let prepare (type a) (method_name, stype) =
    let method_type = (bundle stype : a t) in
    let label = CamlinternalOO.public_method_label method_name in
    let call (x: a) = Obj.magic (CamlinternalOO.send (Obj.magic x) label) in
    Method {method_type; method_name; call}
  in
  let methods = List.map prepare methods |> Array.of_list in
  let find_method = finder (function Method m -> m.method_name) methods in
  { methods; find_method }

let t_of_ttype t = t, lazy (xtype_of_ttype t)

(* builders *)

module Builder = struct

  type 'a t = { mk: 't. ('a, 't) field -> 't } [@@unboxed]
  type 'a named = { mk: 't. ('a, 't) named_field -> 't } [@@unboxed]

  let tuple : type a. a tuple -> a t -> a =
    fun t b ->
      let arity = Array.length t in
      let o = Obj.new_block 0 arity in
      Array.iteri (fun i -> function Field f ->
          Obj.set_field o i (Obj.repr (b.mk f))
        ) t;
      Obj.magic o

  let named_tuple : type a. a named_tuple -> a named -> a =
    fun nt b ->
      let arity = Array.length nt.fields in
      let o = Obj.new_block 0 arity in
      Array.iteri (fun i -> function NamedField f ->
          Obj.set_field o i (Obj.repr (b.mk f))
        ) nt.fields;
      Obj.magic o

  let record : type a. a record -> a named -> a =
    fun (nt, repr) b -> match repr with
      | Regular -> named_tuple nt b
      | Float ->
        let arity = Array.length nt.fields in
        let o = Obj.new_block Obj.double_array_tag arity in
        Array.iteri (fun i -> function NamedField f ->
            Obj.set_double_field o i (Obj.magic (b.mk f))
          ) nt.fields;
        Obj.magic o
      | Unboxed -> begin match nt with
          | {fields =  [|NamedField f|]; _} -> Obj.magic (b.mk f)
          | _ -> assert false
        end

  let constant_constructor : type a. a constant_constructor -> a =
    fun c -> match c.kind with
      | Constant i-> Obj.magic i

  let regular_constructor : type a. a regular_constructor -> a t -> a =
    fun c b -> match c.kind with
      | Regular (t, Tag tag) ->
        let o = Obj.repr (tuple t b) in
        Obj.set_tag o tag; Obj.magic o
      | Regular ([|Field f|], Unboxed) -> Obj.magic (b.mk f)
      | Regular _ -> assert false

  let inlined_constructor : type a. a inlined_constructor -> a named -> a =
    fun c b -> match c.kind with
      | Inlined (nt, Tag tag) ->
        let o = Obj.repr (named_tuple nt b) in
        Obj.set_tag o tag; Obj.magic o
      | Inlined ({fields = [|NamedField f|]; _}, Unboxed) -> Obj.magic (b.mk f)
      | Inlined _ -> assert false
end

module Make = struct

  type 'a t = Obj.t option array
  exception Missing_field of string

  let fresh: int -> 'a t = fun n -> Array.make n None

  let set: 'a t -> ('a, 'b) field -> 'b -> unit =
    fun arr {nth;_} x -> Array.set arr nth (Some (Obj.repr x))

  (* Reuse logic of Builder *)

  let t_builder arr : 'a Builder.t =
    let mk {nth; _} = match arr.(nth) with
      | None -> raise (Missing_field (string_of_int nth))
      | Some o -> Obj.magic o
    in {mk}

  let named_builder arr : 'a Builder.named =
    let mk {field={nth; _}; field_name; _} = match arr.(nth) with
      | None -> raise (Missing_field field_name)
      | Some o -> Obj.magic o
    in {mk}

  let tuple: 'a tuple -> ('a t -> unit) -> 'a =
    fun tup f ->
      let arr = fresh (Array.length tup) in
      f arr;
      Builder.tuple tup (t_builder arr)

  let record: 'a record -> ('a t -> unit) -> 'a =
    fun r f ->
      let arr = fresh (Array.length (fst r).fields) in
      f arr;
      Builder.record r (named_builder arr)

  let constructor: type a. (a, [`Constant | `Inlined | `Regular ]) constructor
    -> (a t -> unit) -> a =
    fun c f -> match c.kind with
      (* TODO: Subtyping *)
      | Constant _ -> Builder.constant_constructor (Obj.magic c)
      | Regular (tup,_) ->
        let arr = fresh (Array.length tup) in
        f arr;
        Builder.regular_constructor (Obj.magic c) (t_builder arr)
      | Inlined (nt,_) ->
        let arr = fresh (Array.length nt.fields) in
        f arr;
        Builder.inlined_constructor (Obj.magic c) (named_builder arr)
end


(* property handling *)

let get_first_props_xtype xt =
  let rec loop accu = function
    | Prop (l, (_, lazy xt)) ->
        loop (l :: accu) xt
    | _ ->
        List.concat (List.rev accu)
  in
  loop [] xt

let rec remove_first_props_xtype : type t. t xtype -> t xtype = function
  | Prop (_, (_, lazy xt)) -> remove_first_props_xtype xt
  | xt -> xt

(* paths *)

module Step = struct
  let cast : (Obj.t, Obj.t) Path.lens -> ('a, 'b) Path.lens = Obj.magic
  let check tag o = Obj.is_block o && Obj.tag o = tag

  let tuple: 'a tuple -> ('a, 'b) field -> ('a,'b) Path.step =
    fun tup f ->
      let arity = Array.length tup in
      let nth = f.nth in
      let get = (fun o -> Some (Obj.field o nth)) in
      let set =
        (fun o v ->
           let o = Obj.dup o in
           Obj.set_field o nth (Obj.repr v); Some o)
      in
      cast { get ; set }, StepMeta.tuple ~nth ~arity


  let regular_constructor:
    'a regular_constructor -> ('a,'b) field -> ('a,'b) Path.step =
    fun c f ->
      let Regular (tup, cstr) = c.kind in
      let arity = Array.length tup in
      let nth = f.nth in
      let get, set = match cstr with
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
      cast { get ; set },
      StepMeta.constructor_regular ~name:c.constr_name ~nth ~arity


  let record: 'a record -> ('a, 'b) named_field -> ('a,'b) Path.step =
    fun (_, repr) nf ->
      let i = nf.field.nth in
      let get, set = match repr with
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
      cast {get; set}, StepMeta.field ~field_name:nf.field_name

  let inlined_constructor:
    'a inlined_constructor -> ('a,'b) named_field -> ('a,'b) Path.step =
    fun c nf ->
      let Inlined (_, repr) = c.kind in
      let i = nf.field.nth in
      let get, set = match repr with
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
      cast {get; set},
      StepMeta.constructor_inline ~name:c.constr_name ~field_name:nf.field_name
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
      | Option (t, _) ->
        let paths = all_paths t target in
        List.map Path.(fun p -> option_step_some t :: p) paths
      | Tuple t -> tuple ~target t
      | Record r -> record ~target r
      | Sum sum ->
        Ext.Array.map_to_list
          (fun c -> match c.kind with
             | Constant _ -> []
             (* TODO: suptyping *)
             | Regular _ -> regular_constructor ~target (Obj.magic c : a regular_constructor)
             | Inlined _ -> inlined_constructor ~target (Obj.magic c : a inlined_constructor)
          ) sum.constructors
        |> List.concat
      | Prop (_, (t,_)) -> all_paths t target
      | Object _ -> []
      | List _ -> []
      | Array _ -> []
      | Function _ -> []
      | Lazy _ -> []
      | Abstract _ -> []

and tuple : type a b. target: b ttype -> a tuple -> (a, b) Path.t list =
  fun ~target tup ->
    Ext.Array.map_to_list
      (function (Field f) ->
         let paths = all_paths (fst f.t) target in
         List.map Path.(fun p -> Step.tuple tup f :: p) paths
      ) tup
    |> List.concat

and record :
  type a b. target: b ttype -> a record -> (a, b) Path.t list =
  fun ~target ((nf,_) as r) ->
    Ext.Array.map_to_list
      (function (NamedField nf) ->
         let paths = all_paths (fst nf.field.t) target in
         List.map Path.(fun p -> Step.record r nf :: p) paths
      ) nf.fields
    |> List.concat

and regular_constructor : type a b.
  target: b ttype -> a regular_constructor -> (a, b) Path.t list =
  fun ~target ({ kind = Regular (tup,_);_} as c) ->
    Ext.Array.map_to_list
      (function (Field f) ->
         let paths = all_paths (fst f.t) target in
         List.map Path.(fun p -> Step.regular_constructor c f :: p) paths
      ) tup
    |> List.concat

and inlined_constructor : type a b.
  target: b ttype -> a inlined_constructor -> (a, b) Path.t list =
  fun ~target ({ kind = Inlined (nt,_);_} as c) ->
    Ext.Array.map_to_list
      (function (NamedField nf) ->
         let paths = all_paths (fst nf.field.t) target in
         List.map Path.(fun p -> Step.inlined_constructor c nf :: p) paths
      ) nt.fields
    |> List.concat

(* Project ttype along path *)

let assert_some = function
  | Some x -> x
  | None -> assert false

let cast : type a b. a t -> b ttype = fun t -> Obj.magic (fst t)

let rec project_path : type a b. a ttype -> (a,b) Path.t -> b ttype =
  fun t -> function
    | [] -> t
    | (_, meta) :: tl ->
      let t = match meta, xtype_of_ttype t with
        | Field {field_name}, Record (nt,_) ->
          (nt.find_field field_name |> assert_some
           |> function NamedField f -> cast f.field.t)
        | Tuple {nth; _}, Tuple fields ->
          (fields.(nth) |> function Field f -> cast f.t)
        | List _, List t -> cast t
        | Array _, Array t -> cast t
        | Constructor {name; arg}, Sum s ->
          ( match arg, (s.find_constructor name |> assert_some).kind with
            | Regular {nth;_}, Regular (t,_) ->
              (t.(nth) |> function Field f -> cast f.t)
            | Inline {field_name}, Inlined (nt,_) ->
              (nt.find_field field_name |> assert_some
               |> function NamedField f -> cast f.field.t)
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

