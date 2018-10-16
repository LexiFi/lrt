module Symbol : sig
  (* A basic type language consist of symbols (with associated fixed arity) and
     variables.

     For such a language, [Symbol.t] corresponds to the set of symbols,
     [Symbol.compare] to a total order on the set of symbols, and
     [Symbol.of_stype] to the destruction of a term into a symbol and a list of
     arguments.

     The total order on symbols, together with the invariant that each symbol
     has a fixed arity, can be used to implement the discrimination tree below.

     This implements a total order on stypes. It assumes, that abstract names
     and [rec_name] of [DT_node]'s are unique identifiers. Further, recursion
     for [DT_node]'s happens only on [rec_args], not on all types of all fields.
     This makes the implementation faster, but stands in contrast to
     [Stype.equality].

     TODO: reduce string comparison to int using [Ext.String.Tbl].
     Therefore, accumulate names during construction of the tree. On
     unification, lookup the name in the table to obtain an integer. Use this
     integer for the total order.

     TODO: Extend stype with an integer symbol identifier to avoid all overhead.
     Serialization of stype would strip this identifier, deserialization would
     lookup the identifier in a registry [Stype.equality] / create a new one.
  *)

  type t
  val compare : t -> t -> int

  type registry
  val empty: registry

  type maybe_free =
    | Symbol of t * Stype.t list (** symbol, arguments *)
    | Var of int (** DT_var *)

  val of_stype: registry -> modulo_props: bool -> Stype.t -> maybe_free option
  val of_stype_register: registry -> modulo_props: bool -> Stype.t -> maybe_free
end = struct
  type base = | Int | Float | String | Array | List | Option | Arrow

  (* TODO: avoid construction of intermediate values and compare stypes
     directly. *)

  (* TODO: Do we really need non-modulo-props matching? *)

  (* TODO: Make Symbol return an int, replace SymbolMap with Hashtable, then
     no compare necessary *)

  type t =
    | Base of base (* TODO: Base could be int too *)
    | Tuple of int
    | Props of Stype.properties
    | Named of int
    | Object of string list (* method names. TODO: may be insufficient *)

  type registry = { mutable n: int
                  ; mutable names: string list
                  ; mutable table: Ext.String.Tbl.t
                  }

  (* let last = ref (-1) *)
  (* let fresh () = incr last; !last *)

  (* let int = fresh ()
  let float = fresh ()
  let string = fresh ()
  let array = fresh ()
      list  *)

  let empty = { n = 0; names = []; table = Ext.String.Tbl.prepare [] }
  let add r name =
    let names = name :: r.names in
    r.n <- r.n + 1;
    r.names <- names;
    r.table <- Ext.String.Tbl.prepare (List.rev names);
    r.n - 1

  let register r s =
    let i = Ext.String.Tbl.lookup r.table s in
    if i >= 0 then i
    else add r s

  let lookup r s =
    let i = Ext.String.Tbl.lookup r.table s in
    if i >= 0 then Some i
    else None

  type maybe_free =
    | Symbol of t * Stype.t list
    | Var of int (* DT_var *)

  let rec of_stype: registry -> modulo_props: bool -> Stype.t -> maybe_free
      option = fun r ~modulo_props -> function
    | DT_int -> Some (Symbol (Base Int, []))
    | DT_float -> Some (Symbol (Base Float, []))
    | DT_string -> Some (Symbol (Base String, []))
    | DT_list a -> Some (Symbol (Base List, [a]))
    | DT_array a -> Some (Symbol (Base Array, [a]))
    | DT_option a -> Some (Symbol (Base Option, [a]))
    | DT_arrow (_, a, b) -> Some (Symbol (Base Arrow, [a; b]))
    | DT_prop (_, s) when modulo_props -> of_stype r ~modulo_props s
    | DT_prop (p, s) -> Some (Symbol (Props p, [s]))
    | DT_tuple l -> Some (Symbol (Tuple (List.length l), l))
    | DT_abstract (rec_name, rec_args)
    | DT_node {rec_name; rec_args; _} ->
      begin match lookup r rec_name with
        | None -> None
        | Some i -> Some (Symbol (Named i, rec_args))
      end
    | DT_object l ->
      let method_names, types = List.split l
      in Some (Symbol (Object method_names, types))
    | DT_var i -> Some (Var i)

  let rec of_stype_register: registry -> modulo_props: bool -> Stype.t ->
    maybe_free = fun r ~modulo_props -> function
    | DT_int -> Symbol (Base Int, [])
    | DT_float -> Symbol (Base Float, [])
    | DT_string -> Symbol (Base String, [])
    | DT_list a -> Symbol (Base List, [a])
    | DT_array a -> Symbol (Base Array, [a])
    | DT_option a -> Symbol (Base Option, [a])
    | DT_arrow (_, a, b) -> Symbol (Base Arrow, [a; b])
    | DT_prop (_, s) when modulo_props -> of_stype_register r ~modulo_props s
    | DT_prop (p, s) -> Symbol (Props p, [s])
    | DT_tuple l -> Symbol (Tuple (List.length l), l)
    | DT_abstract (rec_name, rec_args)
    | DT_node {rec_name; rec_args; _} ->
      Symbol (Named (register r rec_name), rec_args)
    | DT_object l ->
      let method_names, types = List.split l
      in Symbol (Object method_names, types)
    | DT_var i -> Var i

  let compare = compare
  (* TODO: A less naive compare may speed up things significantly. *)
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
     on the first symbol on the stack. Arguments returned by [Symbol.of_stype]
     are pushed back to the stack.

     The set of paths from root to leaves in the resulting tree is homomorphic
     to the set of stypes (modulo Symbol.of_stype). Each path in the tree
     corresponds to one stype and vice versa.

     There are two different types of variables. On type corresponds to
     DT_vars in stypes , the other to free spots in a path. The variables
     in a path are enumerated starting from zero and constitute a normalized
     renaming of the DT_vars. We do that, such that ('a * 'b) and ('b * 'a)
     give the same path of features. During traversal of the tree, we maintain
     a mapping between the two.
  *)

  type key = Stype.t
  type substitution = Stype.t IntMap.t

  module SymbolMap = Map.Make(Symbol)

  type 'a tree =
    | Leave of { value: 'a; free_vars: (int * int) list }
               (* free_vars maps DT_var in stype to free vars in path *)
    | Inner of { map: 'a tree SymbolMap.t; free: 'a tree IntMap.t }

  type 'a t = { modulo_props: bool
              ; tree: 'a tree
              ; registry: Symbol.registry }

  let empty_tree = Inner { map = SymbolMap.empty; free = IntMap.empty }

  let empty ~modulo_props = { modulo_props ;
                              tree = empty_tree ;
                              registry = Symbol.empty}

  let get stype t =
    let modulo_props = t.modulo_props in
    let equal = if modulo_props then
        Stype.equality_modulo_props else Stype.equality
    and get_step s =
      match Symbol.of_stype t.registry ~modulo_props s with
      | Some (Symbol.Symbol (s, l)) -> Some (s, l)
      | None -> None
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
          ( match get_step hd with
            | None -> None
            | Some (step, children) ->
              ( match SymbolMap.find_opt step node.map with
                | None -> None
                | Some x -> traverse (children @ tl) subst x ))
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
        assert false (* This should be impossible. [Symbol.of_stype] should
                        uniquely identify the number of children. *)
    in traverse [stype] [] t.tree

  let add stype value t =
    let get_step =
      let modulo_props = t.modulo_props in
      Symbol.of_stype_register t.registry ~modulo_props
    in
    let rec traverse stack free_vars tree =
      match stack, tree with
      | [], Leave _ ->
        raise (Invalid_argument "(congruent) type already registered")
      | [], Inner {map; free} ->
        (* TODO: can we avoid these asserts by shuffling the code? *)
        assert (SymbolMap.is_empty map);
        assert (IntMap.is_empty free);
        Leave {value; free_vars}
      | hd :: tl, Inner node -> begin
          match get_step hd with
          | Symbol (step, children) ->
            let tree =
              match SymbolMap.find_opt step node.map with
              | None -> empty_tree
              | Some tree -> tree
            in
            let map =
              SymbolMap.add step (traverse (children @ tl) free_vars tree)
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

module type S = sig
  type t
  type 'a data

  val empty: modulo_props:bool -> t
  (** The matcher without any registered pattern. *)

  val add: t: 'a Ttype.t -> 'a data -> t -> t
  (** Add a case to the matcher. *)

  (** {2 Match types with free variables} *)

  module type C0 = sig
    include Unify.T0
    val data : t data
  end

  val add0: (module C0) -> t -> t
  (** Add a case to the matcher. Equivalent to {!add}. *)

  module type C1 = sig
    include Unify.T1
    val data : 'a Ttype.t -> 'a t data
  end

  val add1: (module C1) -> t -> t
  (** Add a case to the matcher. One free variable.*)

  module type C2 = sig
    include Unify.T2
    val data : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t data
  end

  val add2: (module C2) -> t -> t
  (** Add a case to the matcher. Two free variables. *)

  (** {2 Matching Result} *)

  module type M0 = sig
    include Unify.T0
    type matched
    val data : t data
    val eq : (t, matched) TypEq.t
  end

  module type M1 = sig
    include Unify.T1
    type matched
    type a
    val data : a t data
    val eq : (a t, matched) TypEq.t
  end

  module type M2 = sig
    include Unify.T2
    type matched
    type a
    type b
    val data : (a, b) t data
    val eq : ((a, b) t, matched) TypEq.t
  end

  type 'a matched =
    | M0 of (module M0 with type matched = 'a)
    | M1 of (module M1 with type matched = 'a)
    | M2 of (module M2 with type matched = 'a)

  (** {2 Executing the Matcher} *)

  val apply: t -> t: 'a Ttype.t -> 'a matched option
  val apply_exn: t -> t: 'a Ttype.t -> 'a matched
end

module Make (Data: sig type 'a t end) : S with type 'a data = 'a Data.t = struct
  (* TODO: How can I avoid this duplication of signatures? *)
  type 'a data = 'a Data.t

  module type C0 = sig
    include Unify.T0
    val data : t data
  end

  module type C1 = sig
    include Unify.T1
    val data : 'a Ttype.t -> 'a t data
  end

  module type C2 = sig
    include Unify.T2
    val data : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t data
  end

  module type M0 = sig
    include Unify.T0
    type matched
    val data : t data
    val eq : (t, matched) TypEq.t
  end

  module type M1 = sig
    include Unify.T1
    type matched
    type a
    val data : a t data
    val eq : (a t, matched) TypEq.t
  end

  module type M2 = sig
    include C2
    type matched
    type a
    type b
    val data : (a, b) t data
    val eq : ((a, b) t, matched) TypEq.t
  end

  type 'a matched =
    | M0 of (module M0 with type matched = 'a)
    | M1 of (module M1 with type matched = 'a)
    | M2 of (module M2 with type matched = 'a)

  type candidate =
    | C0 of (module C0)
    | C1 of (module C1)
    | C2 of (module C2)

  type t = candidate Tree.t

  let empty ~modulo_props : t = Tree.empty ~modulo_props

  let add (type a) ~(t: a Ttype.t) (data: a data) tree =
    let c = C0 (module struct
        type t = a
        let t = t
        let data = data end)
    in Tree.add (Ttype.to_stype t) c tree

  let add0 (module C : C0) tree =
    Tree.add (Ttype.to_stype C.t) (C0 (module C)) tree

  type var
  let var i : var Ttype.t = Obj.magic (Stype.DT_var i)
  let v0 = var 0
  let v1 = var 1

  let add1 (module C : C1) tree =
    Tree.add (Ttype.to_stype (C.t v0)) (C1 (module C)) tree

  let add2 (module C : C2) tree =
    Tree.add (Ttype.to_stype (C.t v0 v1)) (C2 (module C)) tree

  let ttype: type a. int -> Tree.substitution -> a Ttype.t =
    fun i map ->
      match IntMap.find_opt i map with
      | None -> Obj.magic Std.unit_t (* Unification succeeded, but type variable
                                        was not used. *)
      | Some s -> Obj.magic s (* Unification succeeded by instantiating type
                                 variable with stype s. *)

  let apply : type a. t -> t:a Ttype.t -> a matched option =
    fun tree ~t ->
      let stype = Ttype.to_stype t in
      match Tree.get stype tree with
      | None -> None
      | Some (C0 (module C : C0), map) ->
        assert (IntMap.cardinal map = 0);
        let module M : M0 with type matched = a = struct
          include C
          type matched = a
          let eq = Obj.magic TypEq.refl
        end
        in Some (M0 (module M))
      | Some (C1 (module C : C1), map) ->
        assert (IntMap.cardinal map < 2);
        let module M : M1 with type matched = a = struct
          include C
          type matched = a
          type a
          let eq = Obj.magic TypEq.refl
          let data = data (ttype 0 map)
        end
        in Some (M1 (module M))
      | Some (C2 (module C : C2), map) ->
        assert (IntMap.cardinal map < 3);
        let module M : M2 with type matched = a = struct
          include C
          type matched = a
          type a type b
          let eq = Obj.magic TypEq.refl
          let data = data (ttype 0 map) (ttype 1 map)
        end
        in Some (M2 (module M))

  let apply_exn tree ~t =
    match apply tree ~t with
    | None -> raise Not_found
    | Some m -> m
end
