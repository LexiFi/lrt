module Step : sig
  (* A basic type language consist of symbols (with associated fixed arity) and
     variables.

     For such a language, [Step.t] corresponds to the set of symbols,
     [Step.compare] to a total order on the set of symbols, and [Step.of_stype]
     to the destruction of a term into a symbol and a list of arguments.

     The total order on symbols, together with the invariant that each symbol
     has a fixed arity, can be used to implement the discrimination tree below.

     Unfortunately, [Stype.t] does not carry an identifier of the symbol, thus
     things are a bit complicated. We attempt to read something symbol-like by
     extracting relevant top-level information and return more stypes for
     recursion than theoretically necessary. The recursion into the stype is
     thereby delayed.
  *)

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
    | Record of string * Stype.record_repr * (string * Stype.properties) list
    | Variant of string * Stype.variant_repr * (string * Stype.properties) list
    | Object of string list (* method names *)

  let ignore_props modulo_props =
    if modulo_props then fun _p -> [] else fun p -> p

  let map_record ~modulo_props name flds repr =
    let ig = ignore_props modulo_props in
    let flds, stypes = List.fold_left (fun (flds, stypes) (name, prop, s) ->
        ((name, ig prop) :: flds, s :: stypes)) ([], []) flds
    in Record (name, repr, flds), stypes

  let map_variant ~modulo_props name cstrs repr =
    let ig = ignore_props modulo_props in
    let cstrs, tys = List.fold_left (fun (cstrs, tys) (name, prop, args) ->
        let s = match ( args : _ Stype.variant_args ) with
          | C_tuple stypes -> Stype.DT_tuple stypes
          | C_inline stype -> stype
        in ((name, ig prop) :: cstrs, s :: tys)) ([], []) cstrs
    in Variant (name, repr, cstrs), tys

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
        let step, types = map_record ~modulo_props
            rec_name record_fields record_repr
        in Step (step, types)
        (* TODO: We take all types of all fields and return them for recursion.
           It would be smarter to return only rec_args. But how can we identify
           the record without inspecting the types of all fields? *)
        (* TODO: The same record can be defined twice in different modules.
           This function then yield the same step and as a consequence the
           two records would falsely unify. Solution: insert unique ids on
           [@@deriving t] for (at least) variants, records and abstract types.
           These ids should identify the symbol in the type language. Comparison
           of steps would reduce two the comparison of two ids. Extraction of
           stype arguments would reduce to returning rec_args. *)
        (* The same argument/problems apply to abstract/variant/object. *)
      | DT_node { rec_descr = DT_variant {variant_constrs; variant_repr}
                ; rec_name; _ } ->
        let step, types = map_variant ~modulo_props
            rec_name variant_constrs variant_repr
        in Step (step, types)
      | DT_object l ->
        let method_names, types = List.split l
        in Step (Object method_names, types)
      | DT_var i -> Var i

  let compare = compare
  (* TODO: A less naive compare may speed up things significantly. *)
  (* TODO: Trim stype, such that this compare reduces to comparison of
     integers/uid. Also see comment above.*)

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
     are present, they are used depth-first to further discriminate.

     There are two different types of variables. On type corresponds to
     DT_vars in stypes , the other to free spots in a path. The variables
     in a path are enumerated starting from zero and constitute a normalized
     renaming of the DT_vars. We do that, such that ('a * 'b) and ('b * 'a)
     give the same path of features. During traversal of the tree, we maintain
     a mapping between the two.
  *)

  type key = Stype.t
  type substitution = Stype.t IntMap.t

  module StepMap = Map.Make(Step)

  type 'a tree =
    | Leave of { value: 'a; free_vars: (int * int) list }
               (* free_vars maps DT_var in stype to free vars in path *)
    | Inner of { map: 'a tree StepMap.t; free: 'a tree IntMap.t }

  type 'a t = { modulo_props: bool; tree: 'a tree }

  let empty_tree = Inner { map = StepMap.empty; free = IntMap.empty }

  let empty ~modulo_props = { modulo_props ; tree = empty_tree }

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
        (* undo the variable name normalization *)
        let subst = List.fold_left (fun map (dt_var, free_id) ->
            IntMap.add dt_var (List.assoc free_id subst) map)
            IntMap.empty free_vars
        in Some (value, subst)
      | hd :: tl, Inner node -> begin
          let step, children = get_step hd in
          ( match StepMap.find_opt step node.map with
            | None -> None
            | Some x -> traverse (children @ tl) subst x )
          |>
          (* Ordinary lookup using the outermost feature of the stype hd failed.
             Now try to unifying with the free steps, starting the smallest.
             By doing this, we guarantee (proof?!) that ('a * 'a) is preferred
             over ('a * 'b).
             Rationale: the smallest was bound further up in the path and thus
             is the most restricting choice.
          *)
          ( function
            | Some x -> Some x
            | None ->
              let rec loop seq =
                match seq () with
                | Seq.Nil -> None
                | Seq.Cons ((free_id, tree), rest) ->
                  match List.assoc_opt free_id subst with
                  | None -> traverse tl ((free_id, hd) :: subst) tree
                  | Some stype ->
                    if equal stype hd
                    then traverse tl subst tree
                    else loop rest
              in loop (IntMap.to_seq node.free))
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
      | [], Leave _ ->
        raise (Invalid_argument "(congruent) type already registered")
      | [], Inner {map; free} ->
        (* TODO: can we avoid these asserts by shuffling the code? *)
        assert (StepMap.is_empty map);
        assert (IntMap.is_empty free);
        Leave {value; free_vars}
      | hd :: tl, Inner node -> begin
          match get_step hd with
          | Step (step, children) ->
            let tree =
              match StepMap.find_opt step node.map with
              | None -> empty_tree
              | Some tree -> tree
            in
            let map =
              StepMap.add step (traverse (children @ tl) free_vars tree)
                node.map
            in Inner { node with map }
          | Var dt_var ->
            let free_id =
              (* Was this dt_var already observed further up in the path?
                 If so, reuse free_id, else bump free_id. *)
              match List.assoc_opt dt_var free_vars with
              | Some free_id -> free_id
              | None ->
                  let last =
                    List.fold_left max (-1) (List.rev_map fst free_vars)
                  in last + 1
            in let tree =
              match IntMap.find_opt free_id node.free with
              | Some tree -> tree
              | None -> empty_tree
            in let free =
                 IntMap.add free_id (
                   traverse tl ((dt_var, free_id) :: free_vars) tree)
                   node.free
            in Inner {node with free}
        end
      | _ :: _ , Leave _ -> assert false
    in {t with tree = traverse [stype] [] t.tree}

  let%test _ =
    let open Stype in
    let print_substitution fmt map =
      IntMap.iter (fun i stype ->
          Format.fprintf fmt " DT_var %i -> %a;" i Stype.print stype)
        map
    in
    let print fmt = function
      | None -> Format.fprintf fmt "None"
      | Some (i, s) -> Format.fprintf fmt "Some (%i, %a)" i print_substitution s
    in
    let tadd typ = add (Ttype.to_stype typ) in
    let open Std in
    let t = empty ~modulo_props:true
            |> tadd (list_t int_t) 1
            |> tadd (option_t string_t) 2
            |> tadd int_t 3
            |> add (DT_var 0) 42
            |> add (DT_list (DT_var 0)) 4
            |> add (DT_tuple [DT_var 0; DT_var 0]) 5
            |> add (DT_tuple [DT_var 1; DT_var 0]) 6
            (* this fails as expected *)
            (* |> add (DT_var 1) 42 *)
    in
    let s = Ttype.to_stype in
    List.for_all
      (fun (stype, expected) ->
         let got = get stype t in
         if got = expected then true
         else
         let () =
           Format.printf "expected: %a\ngot: %a\n%!" print expected print got
         in false
      )
      [ s int_t , Some (3, IntMap.empty)
      ; s (list_t string_t), Some (4, IntMap.singleton 0 DT_string)
      ; s (list_t int_t), Some (1, IntMap.empty)
      ; s (option_t string_t) , Some (2, IntMap.empty)
      ; s (list_t (array_t int_t)) ,
        Some (4, IntMap.singleton 0 (DT_array DT_int))
      ; s [%t: (int * int)],
        Some (5, IntMap.singleton 0 DT_int)
      ; s [%t: (int * bool)],
        Some (6, IntMap.(singleton 0 (s bool_t) |> add 1 (s int_t)))
      ; s [%t: int option],
        Some (42, IntMap.singleton 0 (DT_option DT_int))
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
