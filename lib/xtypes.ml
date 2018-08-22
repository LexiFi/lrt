open Types

let cast_ttype: stype -> 'a ttype = Obj.magic

let arrow ?(label = "") t1 t2 = cast_ttype (DT_arrow (label, stype_of_ttype t1, stype_of_ttype t2))

type 'a record_builder = Obj.t option array

let path_of_steps: Path.Internal.step list -> ('a, 'b, 'c) Path.t = Obj.magic

let dummy_xtype = Obj.repr "foo"

module RecordField = struct
  type ('s, 't) t =
    {
      rank: int;
      t: 't ttype;
      name: string;
      props: (string * string) list;

      mutable xtype: Obj.t;
    }

  let ttype r = r.t
  let name r = r.name
  let props r = r.props
  let get r x = Obj.magic (Obj.field (Obj.repr x) r.rank)
  let set r b x = Array.unsafe_set b r.rank (Some (Obj.repr x))
  let path r =
    let s = r.name in
    if s = "" then path_of_steps [Path.Internal.Tuple_nth r.rank]
    else path_of_steps [Path.Internal.Field s]
end

type 's has_record_field = Field: ('s, 't) RecordField.t -> 's has_record_field

type 's field_builder = { mk: 't. ('s, 't) RecordField.t -> 't }
  [@@ocaml.unboxed]

module Record = struct
  type 's t =
    {
      ttype: 's ttype;
      fields: 's has_record_field list;
      make: default:'s option -> ('s record_builder -> unit) -> 's;
      build: 's field_builder -> 's;
      find_field: (string -> 's has_record_field option);
    }

  let ttype r = r.ttype
  let fields r = r.fields
  let make ?default r x = r.make ~default x

  let build r x = r.build x
  let find_field r s = r.find_field s
end

module Constructor = struct
  type ('s, 't) t =
    {
      index: int;
      t: 't ttype;
      name: string;
      props: (string * string) list;
      inline: bool;
      project: 's -> 't;
      inject: 't -> 's;
      path: ('s, 't) Path.constructor;

      mutable xtype: Obj.t;
    }

  let ttype r = r.t
  let index r = r.index
  let name r = r.name
  let props r = r.props
  let inline r = r.inline
  let project_exn r x = r.project x
  let project r x = try Some (r.project x) with Not_found -> None
  let inject r x = r.inject x
  let path r = r.path
end

type 's has_constructor = Constructor: ('s, 't) Constructor.t -> 's has_constructor

module Sum = struct
  type 's t =
    {
      ttype: 's ttype;
      constructors: 's has_constructor array;
      get_constructor_index: 's -> int;
      lookup_constructor: string -> int;
    }

  let ttype x = x.ttype
  let path x =
    match stype_of_ttype x.ttype with
    | DT_node{rec_name=s; _} -> s
    | _ -> assert false

  let constructors x = x.constructors
  let get_constructor_index x y = x.get_constructor_index y
  let lookup_constructor x y = x.lookup_constructor y
  let constructor x y = x.constructors.(x.get_constructor_index y)
end

module Method = struct
  type ('s, 't) t =
    {
     t: 't ttype;
     name: string;
     call: 's -> 't;

     mutable xtype: Obj.t;
    }

  let ttype m = m.t
  let name m = m.name
  let call m x = m.call x
end

type 's has_method = Method: ('s, 't) Method.t -> 's has_method

module Object = struct
  type 's t =
    {
      ttype: 's ttype;
      methods: 's has_method list;
    }

  let ttype o = o.ttype
  let methods o = o.methods
end

type 'a is_function = Function: (string * 'b ttype * 'c ttype) -> ('b -> 'c) is_function
type 's is_list = List: 't ttype -> ('t list) is_list
type 's is_array = Array: 't ttype -> ('t array) is_array
type 's is_option = Option: 't ttype -> ('t option) is_option
type 's is_tuple2 = Tuple2: ('a ttype * 'b ttype) -> ('a * 'b) is_tuple2
type 's is_tuple3 = Tuple3: ('a ttype * 'b ttype * 'c ttype) -> ('a * 'b * 'c) is_tuple3

let is_list (type s_) (s_ttype: s_ ttype) (type t_): s_ is_list option =
  match stype_of_ttype s_ttype with
  | DT_list t_stype -> Some (Obj.magic (List (cast_ttype t_stype: t_ ttype)))
  | _ -> None

let is_array (type s_) (s_ttype: s_ ttype) (type t_): s_ is_array option =
  match stype_of_ttype s_ttype with
  | DT_array t_stype -> Some (Obj.magic (Array (cast_ttype t_stype: t_ ttype)))
  | _ -> None

let is_tuple2 (type s_) (s_ttype: s_ ttype) (type t1_) (type t2_) : s_ is_tuple2 option =
  match stype_of_ttype s_ttype with
  | DT_tuple [t1_stype; t2_stype] ->
      Some (Obj.magic
              (Tuple2 ((cast_ttype t1_stype: t1_ ttype),
                       (cast_ttype t2_stype: t2_ ttype))))
  | _ -> None

let is_tuple3 (type s_) (s_ttype: s_ ttype) (type t1_) (type t2_) (type t3_) : s_ is_tuple3 option =
  match stype_of_ttype s_ttype with
  | DT_tuple [t1_stype; t2_stype; t3_stype] ->
      Some (Obj.magic
              (Tuple3 ((cast_ttype t1_stype: t1_ ttype),
                       (cast_ttype t2_stype: t2_ ttype),
                       (cast_ttype t3_stype: t3_ ttype))))
  | _ -> None

let is_option (type s_) (s_ttype: s_ ttype) (type t_): s_ is_option option =
  match stype_of_ttype s_ttype with
  | DT_option t_stype -> Some (Obj.magic (Option (cast_ttype t_stype: t_ ttype)))
  | _ -> None

let build_object s_ttype methods : _ Object.t =
  let prepare (type t_) (name, t_stype) =
    let t = (cast_ttype t_stype: t_ ttype) in
    let label = CamlinternalOO.public_method_label name in
    let call (x: t_) = Obj.magic (CamlinternalOO.send (Obj.magic x) label) in
    Method {t; name; call; xtype = dummy_xtype}
  in
  {
    ttype = s_ttype;
    methods = List.map prepare methods;
  }

let is_object (type s_) (s_ttype: s_ ttype) : s_ Object.t option =
  match stype_of_ttype s_ttype with
  | DT_object methods -> Some (build_object s_ttype methods)
  | _ -> None

exception Missing_field_in_record_builder

let build_record (type s_) ttype record_repr record_fields : s_ Record.t =
  let len = List.length record_fields in
  let fields =
    List.mapi
      (fun (type t_) i (name, props, t_stype) ->
        let t = (cast_ttype t_stype: t_ ttype) in
        Field
          {
            rank = i;
            t;
            name;
            props;
            xtype = dummy_xtype;
          }
      )
      record_fields
  in
  let make ~default f =
    let b = Array.make len None in
    f b;
    match record_repr with
    | Record_regular | Record_inline _ ->
        (* we could copy b directly except if it is a float array *)
        let r =
          match default with
          | None ->
              let tag = match record_repr with Record_inline tag -> tag | _ -> 0 in
              Obj.new_block tag len
          | Some default -> Obj.dup (Obj.magic default)
        in
        for i = 0 to len - 1 do
          match Array.unsafe_get b i with
          | None -> if default == None then raise Missing_field_in_record_builder
          | Some x -> Obj.set_field r i x
        done;
        Obj.magic r
    | Record_float ->
        let b =
          Array.mapi
            (fun i -> function
              | Some x -> x
              | None ->
                  match default with
                  | Some default -> Obj.magic ((Obj.magic default: float array).(i))
                  | None -> raise Missing_field_in_record_builder
            )
            b
        in
        Obj.magic b (* b is already a float array *)
  in
  let fields_arr = Array.of_list fields in
  let build f =
    match record_repr with
    | Record_regular | Record_inline _ ->
        let tag = match record_repr with Record_inline tag -> tag | _ -> 0 in
        let r = Obj.new_block tag len in
        for i = 0 to len - 1 do
          let (Field field) = Array.unsafe_get fields_arr i in
          Obj.set_field r i (Obj.repr (f.mk field))
        done;
        Obj.magic r
    | Record_float ->
        let r = Array.create_float len in
        for i = 0 to len - 1 do
          let (Field field) = Array.unsafe_get fields_arr i in
          Array.unsafe_set r i (Obj.magic (f.mk field) : float)
        done;
        Obj.magic r
  in
  let tbl = lazy (Ext.String.Tbl.prepare (List.map (fun (Field f) -> f.name) fields)) in
  let find_field s =
    let idx = Ext.String.Tbl.lookup (Lazy.force tbl) s in
    if idx < 0 then None else Some (fields_arr.(idx))
  in
  {ttype; fields; make; build; find_field}

let make len = fun ?default f ->
  let b = Array.make len None in
  f b;
  let r =
    match default with
    | None -> Obj.new_block 0 len
    | Some default -> Obj.dup (Obj.magic default)
  in
  for i = 0 to len - 1 do
    match Array.unsafe_get b i with
    | None -> if default == None then raise Missing_field_in_record_builder
    | Some x -> Obj.set_field r i x
  done;
  Obj.magic r

let makes = Array.init 500 make

let build_tuple (type s_) ttype record_fields : s_ Record.t =
  (* TODO: build the fields_arr directly *)
  let fields =
    List.mapi
      (fun (type t_) i t_stype ->
        let t = (cast_ttype t_stype: t_ ttype) in
        Field {RecordField.rank = i; t; name = ""; props = [];
               xtype = dummy_xtype
              }
      )
      record_fields
  in
  let make = Obj.magic (makes.(List.length record_fields)) in
  let fields_arr = Array.of_list fields in
  let build f =
    let len = Array.length fields_arr in
    let r = Obj.new_block 0 len in
    for i = 0 to len - 1 do
      let (Field field) = Array.unsafe_get fields_arr i in
      Obj.set_field r i (Obj.repr (f.mk field))
    done;
    Obj.magic r
  in
  {ttype; fields; make; build; find_field = (fun _ -> None)}

let is_record s_ttype : _ Record.t option =
  match stype_of_ttype s_ttype with
  | DT_node{rec_descr=DT_record{record_repr; record_fields}; _} ->
      Some (build_record s_ttype record_repr record_fields)
  | _ ->
      None

let is_tuple s_ttype  : _ Record.t option =
  match stype_of_ttype s_ttype with
  | DT_tuple tl ->
      Some (build_tuple s_ttype tl)
  | _ ->
      None

let dup_tag x tag =
  let r = Obj.dup x in
  Obj.set_tag r tag;
  r

let build_sum ttype variant_constrs : _ Sum.t =
  let cst_ids = ref [] in
  let noncst_ids = ref [] in
  let constructors =
    let nb_cst = ref 0 in
    let nb_noncst = ref 0 in
    List.mapi
      (fun (type t_) i (name, props, tl) ->
         let mk nb_args inline stype project inject =
           Constructor
             {
               index = i;
               t = (cast_ttype stype: t_ ttype);
               name;
               props;
               inline;
               project = Obj.magic project;
               inject = Obj.magic inject;
               path = path_of_steps [Path.Internal.Constructor (name, nb_args)];
               xtype = dummy_xtype;
             }
         in
         let mk_noncst nb_args inline stype project inject =
           let tag = !nb_noncst in
           noncst_ids := i :: !noncst_ids;
           incr nb_noncst;
           mk nb_args inline stype
             (fun x -> if Obj.tag x = tag then project x else raise Not_found)
             (inject tag)
         in
         match tl with
         | C_tuple [] ->
             let tag = Obj.magic !nb_cst in
             cst_ids := i :: !cst_ids;
             incr nb_cst;
             mk 0 false (stype_of_ttype Pervasives.unit_t)
               (fun x ->
                  if x == tag then () (* if x is a block, it won't be equal to the tag *)
                  else raise Not_found)
               (fun () -> tag)
         | C_tuple [t] ->
             mk_noncst 1 false t
               (fun x -> Obj.field x 0 (* if x is a constant constructor, Obj.tag x = 1000 *))
               (fun tag x -> let r = Obj.new_block tag 1 in Obj.set_field r 0 x; r)
         | C_tuple tl ->
             mk_noncst (List.length tl) false (DT_tuple tl)
               (fun x -> dup_tag x 0)
               (fun tag x -> dup_tag x tag)
         | C_inline t ->
             mk_noncst 1 true t
               (fun x -> dup_tag x 0)
               (fun tag x -> dup_tag x tag)
      )
      variant_constrs
  in
  let cst_ids = Ext.Array.of_list_rev !cst_ids in
  let noncst_ids = Ext.Array.of_list_rev !noncst_ids in
  let get_constructor_index x =
    let x = Obj.repr x in
    if Obj.is_int x then Array.unsafe_get cst_ids (Obj.magic x)
    else Array.unsafe_get noncst_ids (Obj.tag x)
  in
  let tbl = lazy (Ext.String.Tbl.prepare (List.map (fun (name, _, _) -> name) variant_constrs)) in
  let lookup_constructor s = Ext.String.Tbl.lookup (Lazy.force tbl) s in
  {
    ttype;
    constructors = Array.of_list constructors;
    get_constructor_index;
    lookup_constructor;
  }

let is_sum (type s_) (s_ttype: s_ ttype) : s_ Sum.t option =
  match stype_of_ttype s_ttype with
  | DT_node{rec_name="unit"; _} ->
      None
  | DT_node{rec_descr=DT_variant{variant_constrs}; _} ->
      Some (build_sum s_ttype variant_constrs)
  | _ ->
      None

let is_function (t : 'a ttype) (type t1_) (type t2_) : 'a is_function option =
  match stype_of_ttype t with
  | DT_arrow (s, t1, t2) ->
      let t1_ttype = (cast_ttype t1: t1_ ttype) in
      let t2_ttype = (cast_ttype t2: t2_ ttype) in
      Some (Obj.magic (Function (s, t1_ttype, t2_ttype)))
  | _ -> None

let is_prop (type s_) (s_ttype: s_ ttype) =
  match stype_of_ttype s_ttype with
  | DT_prop (prop, t) -> Some (prop, (cast_ttype t: s_ ttype))
  | _ -> None

let is_abstract (type s_) (s_ttype: s_ ttype) =
  match stype_of_ttype s_ttype with
  | DT_abstract ("lazy_t", [_])
  | DT_abstract (("char" | "int32" | "int64" | "nativeint"), []) -> None
  | DT_abstract (name, args) -> Some (name, s_ttype, args)
  | _ -> None

module type ABSTRACT_1 =
sig
  type 'a t
  val t: unit t ttype
end

let abstract_1_name t =
  match stype_of_ttype t with
  | DT_abstract (s, [DT_node{rec_name="unit"; _}]) -> s
  | x -> failwith (Format.asprintf "Mlfi_xtypes: invalid ABSTRACT_1 witness: %a" Types.print_stype x)

module type ABSTRACT_1_MATCHER_SIG = sig
  type 'a t
  type _ is_t = Is: 'b ttype * ('a, 'b t) TypEq.t -> 'a is_t
  val is_t: 'a ttype -> 'a is_t option
end

module ABSTRACT_1_MATCHER (T : ABSTRACT_1) = struct
  let name = abstract_1_name T.t

  type 'a t = 'a T.t

  type _ is_t = Is: 'b ttype * ('a, 'b T.t) TypEq.t -> 'a is_t

  let is_t (type s) (t : s ttype) : s is_t option =
    match remove_first_props (stype_of_ttype t) with
    | DT_abstract (s, [t1]) when s = name ->
        Some (Is (cast_ttype t1, Obj.magic (TypEq.refl)))
    | _ ->
        None
end

module COMPOSE_ABSTRACT_1_MATCHER (T : ABSTRACT_1_MATCHER_SIG) (S : ABSTRACT_1_MATCHER_SIG) =
struct
  type 'a t = 'a T.t S.t

  type _ is_t = Is: 'b ttype * ('a, 'b T.t S.t) TypEq.t -> 'a is_t

  let is_t t =
    match S.is_t t with
    | Some (S.Is (t, eq2)) ->
        begin match T.is_t t with
        | Some (T.Is (t, eq1)) ->
            let module Lift = TypEq.Lift (struct type 'a c = 'a S.t end) in
            Some (Is (t, TypEq.trans eq2 (Lift.eq eq1)))
        | None ->
            None
        end
    | None ->
        None
end

let make_abstract t =
  match remove_first_props (stype_of_ttype t) with
  | DT_node {rec_name=name; rec_args=l; _} -> Obj.magic (DT_abstract(name, l))
  | _ -> assert false

type 'a xtype
  = Unit: unit xtype
  | Bool: bool xtype
  | Int: int xtype
  | Float: float xtype
  | String: string xtype
  (* | Date: date xtype *)
  | Char: char xtype
  | Int32: int32 xtype
  | Int64: int64 xtype
  | Nativeint: nativeint xtype
  | Option: 'b ttype * 'b xtype Lazy.t -> 'b option xtype
  | List: 'b ttype * 'b xtype Lazy.t -> 'b list xtype
  | Array: 'b ttype * 'b xtype Lazy.t -> 'b array xtype
  | Function: (string * ('b ttype * 'b xtype Lazy.t) * ('c ttype * 'c xtype Lazy.t)) -> ('b -> 'c) xtype
  | Sum: 'a Sum.t -> 'a xtype
  | Tuple: 'a Record.t -> 'a xtype
  | Record: 'a Record.t -> 'a xtype
  | Lazy: ('b ttype * 'b xtype Lazy.t) -> 'b Lazy.t xtype
  | Prop: ((string * string) list * 'a ttype * 'a xtype Lazy.t) -> 'a xtype
  | Object: 'a Object.t -> 'a xtype
  | Abstract: (string * 'a ttype * stype list) -> 'a xtype

let ttype_of_xtype : type t. t xtype -> t ttype = function
  | Unit -> Pervasives.unit_t
  | Bool -> Pervasives.bool_t
  | Int -> Pervasives.int_t
  | Float -> Pervasives.float_t
  | String -> Pervasives.string_t
  (* | Date -> (ttype_of: date) *)
  | Char -> Pervasives.char_t
  | Int32 -> Pervasives.int32_t
  | Int64 -> Pervasives.int64_t
  | Nativeint -> Pervasives.nativeint_t
  | Option (t, _) -> Pervasives.option_t t
  | List (t, _) -> Pervasives.list_t t
  | Array (t, _) -> Pervasives.array_t t
  | Function (label, (t1, _), (t2, _)) -> arrow ~label t1 t2
  | Sum sum -> Sum.ttype sum
  | Tuple r | Record r -> Record.ttype r
  | Lazy (t, _) -> Pervasives.lazy_t_t t
  | Prop (props, t, _) -> add_props props t
  | Object o -> Object.ttype o
  | Abstract (_, t, _) -> t

type Types.memoized_type_prop += Xtype of Obj.t xtype


let rec search a n i =
  if i = n then (Obj.magic Unit)
  else match a.(i) with
  | Xtype r -> r
  | _ -> search a n (i + 1)

let find_memoized_xtype node : 'a xtype =
  Obj.magic (search node.rec_memoized (Array.length node.rec_memoized) 0)

let add_memoized_xtype node xt =
  let s = Xtype (Obj.magic xt) in
  let old = node.rec_memoized in
  let a = Array.make (Array.length old + 1) s in
  Array.blit old 0 a 1 (Array.length old);
  Types.Internal.set_memoized node a;
  xt

let rec xtype_of_ttype (type s_) (s: s_ ttype) : s_ xtype =
  (* This function is used quite a lot (e.g. in the "cst" combinator),
     so we accept some internal unsafety to improve performance. *)
  match stype_of_ttype s with
  | DT_node{rec_name="unit"; _} -> Obj.magic Unit
  | DT_node{rec_name="bool"; _} -> Obj.magic Bool
  | DT_int -> Obj.magic Int
  | DT_float -> Obj.magic Float
  | DT_string -> Obj.magic String
  (* | DT_date -> Obj.magic Date *)
  | DT_list t -> Obj.magic (List (cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_array t -> Obj.magic (Array (cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_option t -> Obj.magic (Option (cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_arrow (l, t1, t2) -> Obj.magic (Function (l, (cast_ttype t1, lazy (xtype_of_ttype (cast_ttype t1))),
                                                    (cast_ttype t2, lazy (xtype_of_ttype (cast_ttype t2)))))
  | DT_prop (p, t) -> Obj.magic (Prop (p, cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_abstract ("lazy_t", [t]) -> Obj.magic (Lazy (cast_ttype t, lazy (xtype_of_ttype (cast_ttype t))))
  | DT_abstract ("char", []) -> Obj.magic Char
  | DT_abstract ("int32", []) -> Obj.magic Int32
  | DT_abstract ("int64", []) -> Obj.magic Int64
  | DT_abstract ("nativeint", []) -> Obj.magic Nativeint
  | DT_tuple tl -> Tuple (build_tuple s tl)
  | DT_node({rec_descr=DT_record{record_repr; record_fields}; _} as node) ->
      begin match find_memoized_xtype node with
      | Unit -> add_memoized_xtype node (Record (build_record s record_repr record_fields))
      | r -> Obj.magic r
      end

  | DT_node({rec_descr=DT_variant{variant_constrs}; _} as node) ->
      begin match find_memoized_xtype node with
      | Unit -> add_memoized_xtype node (Sum (build_sum s variant_constrs))
      | r -> Obj.magic r
      end

  | DT_object methods ->
      Object (build_object s methods)

  | DT_abstract (name, args) -> Abstract (name, s, args)

  | DT_var _ ->
      assert false

let xtype_of_field (r : (_, 'a) RecordField.t) : 'a xtype =
  if r.xtype != dummy_xtype then
    Obj.magic r.xtype
  else begin
    let xt = xtype_of_ttype r.t in
    r.xtype <- Obj.repr xt;
    xt
  end

let xtype_of_constructor (r : (_, 'a) Constructor.t) : 'a xtype =
  if r.xtype != dummy_xtype then
    Obj.magic r.xtype
  else begin
    let xt = xtype_of_ttype r.t in
    r.xtype <- Obj.repr xt;
    xt
  end

let xtype_of_method (r : (_, 'a) Method.t) : 'a xtype =
  if r.xtype != dummy_xtype then
    Obj.magic r.xtype
  else begin
    let xt = xtype_of_ttype r.t in
    r.xtype <- Obj.repr xt;
    xt
  end

let get_first_props_xtype xt =
  let rec loop accu = function
    | Prop (l, _, lazy xt) ->
        loop (l :: accu) xt
    | _ ->
        List.concat (List.rev accu)
  in
  loop [] xt

let rec remove_first_props_xtype : type t. t xtype -> t xtype = function
  | Prop (_, _, lazy xt) -> remove_first_props_xtype xt
  | xt -> xt

type sttype = Ttype: 'a ttype -> sttype

let sttype_of_stype s = Ttype (Obj.magic s)

let rec all_paths: type root target. root:root ttype -> target:target ttype -> (root, target, _) Path.t list = fun ~root ~target ->
  match ttypes_equality root target with
  | Some TypEq.Eq -> Obj.magic ([])
  | None ->
      match xtype_of_ttype root with
      | Unit -> []
      | Bool -> []
      | Int -> []
      | Float -> []
      | String -> []
      (* | Date -> [] *)
      | Char -> []
      | Int32 -> []
      | Int64 -> []
      | Nativeint -> []
      | Option (t, _) ->
          let paths = all_paths ~root:t ~target in
          (* List.map (Path.(^^) (.Some)) paths *)
          (* TODO: Patrik's wild guess *)
          List.map Path.(
              (^^) (Internal.Constructor ("Some", 1) |> Obj.magic)) paths
      | Tuple record
      | Record record ->
          List.map
            (function (Field f) ->
              let paths = all_paths ~target ~root:(RecordField.ttype f) in
              List.map (Path.(^^) (RecordField.path f)) paths)
            (Record.fields record)
          |> List.concat
      | Sum sum ->
          List.concat
            (Ext.Array.map_to_list
               (function (Constructor c) ->
                 let paths = all_paths ~target ~root:(Constructor.ttype c) in
                 List.map (Path.(^^) (Constructor.path c)) paths)
               (Sum.constructors sum))
      | Prop (_, t, _) -> all_paths ~target ~root:t
      | Object _ -> []
      | List _ -> []
      | Array _ -> []
      | Function _ -> []
      | Lazy _ -> []
      | Abstract _ -> []
