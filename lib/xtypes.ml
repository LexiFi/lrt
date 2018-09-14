open Dynt_core
open Dynt_core.Ttype
open Dynt_core.Stype

type 'a t = 'a ttype * 'a xtype Lazy.t

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
  ; step: ('s, 't) Path.step (* TODO: should we make this lazy? *)
  }

and 's has_field = Field: ('s, 't) field -> 's has_field

and 's tuple = 's has_field array

and ('s, 't) named_field =
  { field: ('s, 't) field
  ; field_name: string
  ; field_props: stype_properties
  }

and 's has_named_field = NamedField: ('s, 't) named_field -> 's has_named_field

and 's record =
  { fields: 's has_named_field array
  ; find_field: string -> 's has_named_field option
  }

and 's constructor_kind =
  | Constant
  | Regular of 's tuple
  | Inlined of 's record

and 's constructor =
  { constructor_name: string
  ; constructor_props: stype_properties
  ; kind: 's constructor_kind
  }

and 's sum =
  { constructors: 's constructor array
  ; find_constructor: string -> 's constructor option
  ; constructor: 's -> 's constructor
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

and 's object_ =
  { methods : 's has_method array
  ; find_method : string -> 's has_method option
  }

and 's has_method = Method: ('s, 't) method_ -> 's has_method

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

type constructor_repr = Unboxed | VTag of int

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
  | DT_node ({rec_descr = DT_variant variant; _} as node) ->
    begin match is_memoized node with
    | None -> memoize node (
          cast_xtype (xtype_of_variant variant))
    | Some xt -> cast_xtype xt
    end
  | DT_object _ -> assert false (* TODO *)
  | DT_prop (props, s) -> Prop(props, bundle s)
  | DT_abstract (name, l) -> Abstract (name, l)
  | DT_var _ -> assert false

and bundle : type a. stype -> a t = fun s ->
    let t = cast_ttype s
    in (t, lazy (xtype_of_ttype t))

and tuple ?cstr ?(meta=StepMeta.tuple) fields =
  let check tag o = Obj.is_block o && Obj.tag o = tag in
  let get, set = match cstr with
    (* TODO: Can this code be reused for the record case below? *)
    | Some Unboxed ->
      (fun _i o -> Some o),
      (fun _i _o v -> Some v)
    | Some (VTag tag) ->
      (fun i o -> if check tag o then Some (Obj.field o i) else None),
      (fun i o v -> if check tag o then
          let o = Obj.dup o in
          Obj.set_field o i (Obj.repr v); Some o
        else None)
    | None ->
      (fun i o -> Some (Obj.field o i)),
      (fun i o v ->
          let o = Obj.dup o in
          Obj.set_field o i (Obj.repr v); Some o)
  in
  let arity = List.length fields in
  let fields = List.mapi (fun i t ->
      Field { t = bundle t
            ; step = { get = get i ; set = set i },
                     meta ~nth:i ~arity }
    ) fields
  in (Array.of_list fields : 'a has_field array)

and record ?cstr ?(meta=StepMeta.field) record : 'a record =
  let check tag o = Obj.is_block o && Obj.tag o = tag in
  let get, set =
    match cstr, record.record_repr with
    | None, Record_float ->
      ( fun i r -> Some (Obj.repr (Obj.double_field r i))),
      ( fun i r v ->
          let r = Obj.dup r in
          Obj.set_double_field r i (Obj.magic v); Some r)
    | Some (VTag tag), Record_float ->
      ( fun i r -> if check tag r then
          Some (Obj.repr (Obj.double_field r i)) else None ),
      ( fun i r v ->
          let r = Obj.dup r in
          Obj.set_double_field r i (Obj.magic v); Some r)
    | None, Record_regular ->
      (fun i r -> Some (Obj.field r i)),
      (fun i r v ->
         let r = Obj.dup r in
         Obj.set_field r i (Obj.repr v); Some r)
    (* TODO: We do not need the tag here. Can we drop it from the stype ? *)
    | Some (VTag tag), Record_inline _->
      (fun i r -> if check tag r then
          Some (Obj.field r i) else None),
      (fun i r v -> if check tag r then
          let r = Obj.dup r in
          Obj.set_field r i (Obj.repr v); Some r
        else None)
    | None, Record_unboxed
    | Some Unboxed, Record_unboxed ->
      (fun _i r -> Some (Obj.repr r)),
      (fun _i _r v -> Some (Obj.repr v))
    (* TODO: Parts of record representation seem superfluous with this new
       xtype implementation. Should ditch from stype? *)
    (* Boxed record within unboxed constructor *)
    | Some Unboxed, _ -> assert false
    (* Unboxed record inside boxed constructor *)
    | Some (VTag _), Record_unboxed -> assert false
    (* Inline record outside constructor *)
    | None, Record_inline _ -> assert false
    (* Regular record as variant argument *)
    | Some (VTag _), Record_regular -> assert false
  in
  let fields = List.mapi (fun i (field_name, field_props, s) ->
      let meta = meta ~field_name in
      NamedField { field_name; field_props;
                   field = { t = bundle s
                           ; step = { get = get i
                                    ; set = set i }, meta }}
    ) record.record_fields
  in
  let tbl = lazy (
    Ext.String.Tbl.prepare
      (List.map (fun (NamedField f) -> f.field_name) fields))
  in
  let fields = Array.of_list fields in
  let find_field s =
    let i = Ext.String.Tbl.lookup (Lazy.force tbl) s in
    if i < 0 then None else Some fields.(i)
  in { fields; find_field }

and xtype_of_variant variant : 'a xtype =
  let cstr = match variant.variant_repr with
    | Variant_unboxed -> fun _ -> Unboxed
    | Variant_regular -> fun tag -> VTag tag
  in
  let n = List.length variant.variant_constrs in
  let constructors = Array.make n
      { constructor_name = ""
      ; constructor_props = []
      ; kind = Constant }
  in
  let cst, ncst = ref [], ref [] in
  List.iteri (fun i (name, constructor_props, arg) ->
      let kind =
        match arg with
        | C_tuple [] -> cst := i :: !cst ; Constant
        | C_tuple l ->
          ncst := i :: !ncst;
          let meta = StepMeta.constructor_regular ~name in
          let cstr = cstr i in
          Regular (tuple ~cstr ~meta l)
        | C_inline (DT_node {rec_descr = DT_record r; _}) ->
          ncst := i :: !ncst;
          let meta = StepMeta.constructor_inline ~name in
          let cstr = cstr i in
          Inlined (record ~cstr ~meta r)
        | C_inline _ -> assert false
      in
      constructors.(i) <- {kind ; constructor_props; constructor_name = name}
    ) variant.variant_constrs;
  (* Lookup tables tag -> constructor index *)
  let cst = Ext.Array.of_list_rev !cst in
  let ncst = Ext.Array.of_list_rev !ncst in
  (* Constructor by value *)
  let constructor x : 'a constructor =
    let i =
      let x = Obj.repr x in
      if Obj.is_int x then Array.get cst (Obj.magic x)
      else Array.get ncst (Obj.tag x)
    in constructors.(i)
  in
  (* Constructor by name *)
  let tbl = lazy (
    Ext.String.Tbl.prepare
      (Ext.Array.map_to_list (fun c -> c.constructor_name) constructors))
  in
  let find_constructor s =
    let i = Ext.String.Tbl.lookup (Lazy.force tbl) s in
    if i < 0 then None else Some constructors.(i)
  in Sum { constructors; find_constructor; constructor }

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
             | Constant -> []
             | Regular t -> tuple ~target t
             | Inlined r -> record ~target r
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
  fun ~target t ->
    Ext.Array.map_to_list
      (function (Field f) ->
         let paths = all_paths (fst f.t) target in
         List.map Path.(fun p -> f.step :: p) paths
      ) t
    |> List.concat

and record : type a b. target: b ttype -> a record -> (a, b) Path.t list =
  fun ~target r ->
    Ext.Array.map_to_list
      (function (NamedField nf) ->
         let paths = all_paths (fst nf.field.t) target in
         List.map Path.(fun p -> nf.field.step :: p) paths
      ) r.fields
    |> List.concat


let project_path ~t:_ _p = assert false (* TODO *)

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

