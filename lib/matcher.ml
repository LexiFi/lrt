module Step : sig
  type t
  val compare : t -> t -> int

  type maybe_free =
    | Step of t * Stype.t list
    | Var of int (* DT_var *)

  val of_stype : modulo_props: bool -> Stype.t -> maybe_free
end = struct
  type base = | Int | Float | String | Array | List | Option | Arrow
  type t =
    | Base of base
    | Tuple of int
    | Props of Stype.properties
    | Abstract of int * string (* arity, name *)
    | Record of string * Stype.record_repr * ( string * Stype.properties) list

  let map_record name flds repr =
    let flds, stypes = List.fold_left (fun (flds, stypes) (name, prop, s) ->
        ((name, prop) :: flds, s :: stypes)) ([], []) flds
    in (name, repr, flds), stypes

  type maybe_free =
    | Step of t * Stype.t list
    | Var of int (* DT_var *)

  let rec of_stype: modulo_props: bool -> Stype.t -> maybe_free =
    fun ~modulo_props -> function
      | DT_int -> Step (Base Int, [])
      | DT_float -> Step (Base Float, [])
      | DT_string -> Step (Base String, [])
      | DT_list a -> Step (Base List, [a])
      | DT_array a -> Step (Base Array, [a])
      | DT_option a -> Step (Base Option, [a])
      | DT_arrow (_, a, b) -> Step (Base Arrow, [a; b])
      | DT_prop (_, s) when modulo_props -> of_stype ~modulo_props s
      | DT_prop (p, s) -> Step (Props p, [s])
      | DT_tuple l -> Step (Tuple (List.length l), l)
      | DT_abstract (name, args) ->
        Step (Abstract (List.length args, name), args)
      | DT_node { rec_descr = DT_record {record_fields; record_repr}
                ; rec_name; _ } ->
        let (name, repr, flds), types =
          map_record rec_name record_fields record_repr
        in
        (* TODO: verify, that rec_args are indeed irrelevant *)
        (* TODO: The same record can be defined twice in different modules
           and pass this comparison. Solution: insert unique ids on
           [@@deriving t]. Or check what the existing rec_uid is doing.
           This would speed up comparison quite a bit, ie. only args need
           to be compared *)
        Step (Record (name, repr, flds), types)
      | DT_node _ -> failwith "TODO: handle variants"
      | DT_object _ -> failwith "TODO: handle objects"
      | DT_var i -> Var i

  let compare = compare
  (* TODO: A less naive compare may speed up things significantly. *)
  (* TODO: Trim stype, such that this compare reduces to comparison of
     integers/uid *)

end

module IntMap = Map.Make (struct type t = int let compare = compare end)

module Tree : sig
  type 'a t
  type key = Stype.t
  type substitution = Stype.t IntMap.t
  val empty : modulo_props: bool -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val get : key -> 'a t -> ('a * substitution) option
end = struct
  (* On each level of the discrimination tree, we discriminate on the
     outermost structure of the stype using [Step.of_stype]. When stype children
     are present, they are used depth-first to further discriminate. *)

  type key = Stype.t
  type substitution = Stype.t IntMap.t

  module Map = Map.Make(Step)

  type 'a tree =
    | Leave of { value: 'a; free_vars: (int * int) list }
               (* free_vars maps DT_var in stype to id's of free nodes *)
    | Inner of { map: 'a tree Map.t
               ; free: (int * 'a tree) option }

  type 'a t = { modulo_props: bool
              ; tree: 'a tree
              ; mutable last_free: int }

  let empty ~modulo_props = { modulo_props
                            ; tree = Inner { map = Map.empty
                                           ; free = None }
                            ; last_free = -1
                            }

  exception Not_unifiable

  let get stype t =
    let modulo_props = t.modulo_props in
    let equal = if modulo_props then
        Stype.equality_modulo_props else Stype.equality
    and get_step s =
      match Step.of_stype ~modulo_props s with
      | Step.Step (s, l) -> (s, l)
      | _ -> failwith "free variable in query"
    in
    let rec traverse stack subst tree =
      match stack, tree with
      | [], Leave {value; free_vars} ->
        begin try
            let subst = List.fold_left (fun map (dt_var, node_id) ->
                let stype = List.assoc node_id subst in
                match IntMap.find_opt dt_var map with
                | None -> IntMap.add dt_var stype map
                | Some s ->
                  if not (equal s stype) then raise Not_unifiable else map
              ) IntMap.empty free_vars
            in Some (value, subst)
          with Not_unifiable -> None end
      | hd :: tl, Inner node -> begin
          let step, children = get_step hd in
          match Map.find_opt step node.map with
          | Some tree -> traverse (children @ tl) subst tree
          | None -> match node.free with
            | None -> None
            | Some (node_id, tree) -> traverse tl ((node_id, hd) :: subst) tree
        end
      | [], _
      | _ :: _, Leave _ ->
        assert false (* This should be impossible. [Step.of_stype] should
                        uniquely identify the number of children. *)
    in traverse [stype] [] t.tree

  let add stype value t =
    let get_step =
      let modulo_props = t.modulo_props in
      Step.of_stype ~modulo_props
    in
    let rec traverse stack free_vars tree =
      match stack, tree with
      | [], Leave _ -> raise (Invalid_argument "type already registered")
      | [], Inner {map; free} ->
        (* TODO: can we avoid these asserts by shuffling the code? *)
        assert (Map.is_empty map);
        assert (free = None);
        Leave {value; free_vars}
      | hd :: tl, Inner node -> begin
          match get_step hd with
          | Var dt_var ->
            let node_id, tree = match node.free with
              | Some (node_id, tree) -> (node_id, tree)
              | None ->
                let node_id = succ t.last_free in
                t.last_free <- node_id;
                let tree = Inner {map = Map.empty; free = None} in
                (node_id, tree)
            in
            let free =
                let tree = traverse tl ((dt_var, node_id) :: free_vars) tree in
                Some (node_id, tree)
            in Inner {node with free}
          | Step (step, children) ->
            let tree =
              match Map.find_opt step node.map with
              | None -> Inner {map = Map.empty; free = None}
              | Some tree -> tree
            in
            let map =
              Map.add step (traverse (children @ tl) free_vars tree) node.map
            in Inner { node with map }
        end
      | _ :: _ , Leave _ -> assert false
    in {t with tree = traverse [stype] [] t.tree}

  let%test _ =
    let tadd typ = add (Ttype.to_stype typ) in
    let tget typ = get (Ttype.to_stype typ) in
    let open Std in
    let t = empty ~modulo_props:true
            |> tadd (list_t int_t) 1
            |> tadd (option_t string_t) 2
            |> tadd int_t 3
            |> add (DT_list (DT_var 0)) 4
            |> add (DT_tuple [DT_var 0; DT_var 0]) 5
            (* TODO: this must work *)
            (* |> add (DT_tuple [DT_var 1; DT_var 0]) 5 *)
            |> add (DT_var 0) 42
            (* this fails correctly *)
            (* |> add (DT_var 1) 42 *)
    in
    let open Stype in
    List.for_all (fun x -> x)
      [ tget int_t t = Some (3, IntMap.empty)
      ; tget (list_t string_t) t = Some (4, IntMap.singleton 0 DT_string)
      ; tget (list_t int_t) t = Some (1, IntMap.empty)
      ; tget (option_t string_t) t = Some (2, IntMap.empty)
      ; tget (list_t (array_t int_t)) t =
        Some (4, IntMap.singleton 0 (DT_array DT_int))
      ; tget [%t: (int * int)] t =
        Some (5, IntMap.singleton 0 DT_int)
      ; tget [%t: (int * bool)] t = None
      ; tget (option_t int_t) t = None (* TODO: Why is this not Some 42 *)
      ]
end

module type C0 = sig
  include Unify.T0
  type res
  val f: t -> res
end

module type C1 = sig
  include Unify.T1
  type res
  val f: 'a Ttype.t -> 'a t -> res
end

module type C2 = sig
  include Unify.T2
  type res
  val f: 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t -> res
end

type 'a candidate =
  | T0 of (module C0 with type res = 'a)
  | T1 of (module C1 with type res = 'a)
  | T2 of (module C2 with type res = 'a)

type 'a t = 'a candidate Tree.t

let empty ~modulo_props : 'a t = Tree.empty ~modulo_props

let add (type t res) ~(t: t Ttype.t) ~(f: t -> res) tree =
  let c = T0 (module struct
      type nonrec t = t [@@deriving t]
      type nonrec res = res
      let f = f end)
  in Tree.add (Ttype.to_stype t) c tree

let add0 (type a) (module C : C0 with type res = a) tree =
  Tree.add (Ttype.to_stype C.t) (T0 (module C)) tree

type var
let var i : var Ttype.t = Obj.magic (Stype.DT_var i)
let v0 = var 0
let v1 = var 1

let add1 (type a) (module C : C1 with type res = a) tree =
  Tree.add (Ttype.to_stype (C.t v0)) (T1 (module C)) tree

let add2 (type a) (module C : C2 with type res = a) tree =
  Tree.add (Ttype.to_stype (C.t v0 v1)) (T2 (module C)) tree

let ttype: type a. int -> Stype.t IntMap.t -> a Ttype.t =
  fun i map ->
    match IntMap.find_opt i map with
    | None -> Obj.magic Std.unit_t (* Unification succeeded, but type variable
                                      was not used. *)
    | Some s -> Obj.magic s (* Unification succeeded by instantiating type
                               variable with stype s. *)

let apply' : type res. res t -> Ttype.dynamic -> res =
  fun tree (Ttype.Dyn (t,x)) ->
    let stype = Ttype.to_stype t in
    match Tree.get stype tree with
    | None -> raise Not_found
    | Some (T0 (module M : C0 with type res = res), map) ->
      assert (IntMap.cardinal map = 0);
      M.f (Obj.magic x)
    | Some (T1 (module M : C1 with type res = res), map) ->
      assert (IntMap.cardinal map < 2);
      M.f (ttype 0 map) (Obj.magic x)
    | Some (T2 (module M : C2 with type res = res), map) ->
      assert (IntMap.cardinal map < 3);
      M.f (ttype 0 map) (ttype 1 map) (Obj.magic x)

let apply matcher ~t x = apply' matcher (Ttype.Dyn (t, x))
