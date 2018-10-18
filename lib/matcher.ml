module type SYMBOL = sig
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

     TODO: Update documentation. Especially, the prop handling: key only

     TODO: may property names and rec_name/abstract name collide?
  *)

  type maybe_free =
    | Symbol of int * Stype.t list  (** symbol, arguments *)
    | Var of int  (** DT_var *)

  val of_stype : modulo_props:bool -> Stype.t -> maybe_free option
  val of_stype_register : modulo_props:bool -> Stype.t -> maybe_free
end

module Symbol (Unit : sig end) : SYMBOL = struct
  (* TODO: Do we really need non-modulo-props matching? *)

  let strings = ref []
  let table = ref (Ext.String.Tbl.prepare [])

  (* Strings are mapped to positive non-neg integers using a string table *)

  let last = ref 0
  let next () = decr last ; !last

  (* Non-strings are mapped to negative integers using a counter *)

  let int = next ()
  let float = next ()
  let string = next ()
  let array = next ()
  let list = next ()
  let option = next ()
  let arrow = next ()

  (* TODO: make this grow on demand? *)
  let tuples = Array.init 10 (fun _ -> next ())

  let add name =
    strings := name :: !strings ;
    (* reverse, otherwise old strings would get new ids *)
    table := Ext.String.Tbl.prepare (List.rev !strings)

  let lookup s =
    let i = Ext.String.Tbl.lookup !table s in
    if i >= 0 then Some i else None

  let register s =
    let i = Ext.String.Tbl.lookup !table s in
    if i >= 0 then i
    else (
      add s ;
      match lookup s with
      | None -> failwith "broken logic in Matcher.Symbol"
      | Some i -> i )

  type maybe_free = Symbol of int * Stype.t list | Var of int

  (* DT_var *)
  
  (* Associativity by enforcing single prop DT_prop *)
  let rec normalize_props t = function
    | [] -> t
    | hd :: tl -> normalize_props (Stype.DT_prop ([hd], t)) tl

  let rec of_stype : modulo_props:bool -> Stype.t -> maybe_free option =
   fun ~modulo_props -> function
    | DT_int -> Some (Symbol (int, []))
    | DT_float -> Some (Symbol (float, []))
    | DT_string -> Some (Symbol (string, []))
    | DT_list a -> Some (Symbol (list, [a]))
    | DT_array a -> Some (Symbol (array, [a]))
    | DT_option a -> Some (Symbol (option, [a]))
    | DT_arrow (_, a, b) -> Some (Symbol (arrow, [a; b]))
    | DT_prop (_, s) when modulo_props -> of_stype ~modulo_props s
    | DT_prop ([], s) -> of_stype ~modulo_props s
    | DT_prop ([(key, _)], s) -> (
      match lookup key with None -> None | Some i -> Some (Symbol (i, [s])) )
    | DT_prop (l, s) -> of_stype ~modulo_props (normalize_props s l)
    | DT_tuple l ->
        let arity = List.length l in
        if arity < Array.length tuples then Some (Symbol (tuples.(arity), l))
        else None
    | DT_abstract (rec_name, rec_args) | DT_node {rec_name; rec_args; _} -> (
      match lookup rec_name with
      | None -> None
      | Some i -> Some (Symbol (i, rec_args)) )
    | DT_object _ -> failwith "object not supported"
    | DT_var i -> Some (Var i)

  let rec of_stype_register : modulo_props:bool -> Stype.t -> maybe_free =
   fun ~modulo_props -> function
    | DT_int -> Symbol (int, [])
    | DT_float -> Symbol (float, [])
    | DT_string -> Symbol (string, [])
    | DT_list a -> Symbol (list, [a])
    | DT_array a -> Symbol (array, [a])
    | DT_option a -> Symbol (option, [a])
    | DT_arrow (_, a, b) -> Symbol (arrow, [a; b])
    | DT_prop (_, s) when modulo_props -> of_stype_register ~modulo_props s
    | DT_prop ([], s) -> of_stype_register ~modulo_props s
    | DT_prop ([(key, _)], s) -> Symbol (register key, [s])
    | DT_prop (l, s) -> of_stype_register ~modulo_props (normalize_props s l)
    | DT_tuple l ->
        let arity = List.length l in
        if arity < Array.length tuples then Symbol (tuples.(arity), l)
        else failwith "tuple too long"
    | DT_abstract (rec_name, rec_args) | DT_node {rec_name; rec_args; _} ->
        Symbol (register rec_name, rec_args)
    | DT_object _ -> failwith "object not supported"
    | DT_var i -> Var i
end

(*
let%expect_test _ =
  let module A = Symbol (struct end) in
  let open A in
  let open Stype in
  let print = function
    | Var i -> Printf.printf "Var %i\n%!" i
    | Symbol (i,_) -> Printf.printf "Symbol %i\n%!" i
  and modulo_props = true in
  let print' = function
    | None -> Printf.printf "not registered\n%!"
    | Some x -> print x
  in
  print (of_stype_register ~modulo_props DT_int);
  print (of_stype_register ~modulo_props (DT_array DT_int));
  print (of_stype_register ~modulo_props (DT_abstract ("a",[])));
  print (of_stype_register ~modulo_props (DT_abstract ("c",[])));
  print (of_stype_register ~modulo_props (DT_abstract ("d",[])));
  print' (of_stype ~modulo_props DT_int);
  print' (of_stype ~modulo_props (DT_tuple [DT_int; DT_int]));
  print' (of_stype ~modulo_props (DT_tuple []));
  print' (of_stype ~modulo_props (DT_abstract ("a",[])));
  print' (of_stype ~modulo_props (DT_abstract ("b",[])));
  print' (of_stype ~modulo_props (DT_abstract ("c",[])));
  print' (of_stype ~modulo_props (DT_abstract ("d",[])));
  print (of_stype_register ~modulo_props (DT_abstract ("b",[])));
  print' (of_stype ~modulo_props (DT_abstract ("b",[])));
  let module B = Symbol (struct end) in
  let open B in
  let print = function
    | Var i -> Printf.printf "Var %i\n%!" i
    | Symbol (i,_) -> Printf.printf "Symbol %i\n%!" i
  and modulo_props = true in
  let print' = function
    | None -> Printf.printf "not registered\n%!"
    | Some x -> print x
  in
  print' (of_stype ~modulo_props (DT_abstract ("b",[])));
  [%expect {|
    Symbol -1
    Symbol -4
    Symbol 0
    Symbol 1
    Symbol 2
    Symbol -1
    Symbol -10
    Symbol -8
    Symbol 0
    not registered
    Symbol 1
    Symbol 2
    Symbol 3
    Symbol 3
    not registered |}]
*)

module IntTable = struct
  type 'a t = (int, 'a) Hashtbl.t

  open Hashtbl

  let create () = create 5
  let set = replace
  let get = find_opt
  let fold = fold
end

module IntMap = Map.Make (struct type t = int

                                 let compare = compare end)

module Tree (Symbol : SYMBOL) : sig
  type 'a t
  type key = Stype.t
  type substitution = Stype.t IntMap.t

  val create : modulo_props:bool -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val get : 'a t -> key -> ('a * substitution) option
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

  type 'a tree =
    | Leave of {value: 'a; free_vars: (int * int) list}
    (* free_vars maps DT_var in stype to free vars in path *)
    | Inner of {steps: 'a tree IntTable.t; free: 'a tree IntTable.t}

  type 'a t = {modulo_props: bool; tree: 'a tree}

  let create_node () =
    Inner {steps= IntTable.create (); free= IntTable.create ()}

  let create ~modulo_props = {modulo_props; tree= create_node ()}

  let get t stype =
    let modulo_props = t.modulo_props in
    let equal =
      if modulo_props then Stype.equality_modulo_props else Stype.equality
    and get_step s =
      match Symbol.of_stype ~modulo_props s with
      | Some (Symbol.Symbol (s, l)) -> Some (s, l)
      | None -> None
      | _ -> failwith "free variable in query"
    in
    let rec traverse stack subst tree =
      match (stack, tree) with
      | [], Leave {value; free_vars} ->
          (* undo the variable name normalization *)
          let subst =
            List.fold_left
              (fun map (dt_var, free_id) ->
                IntMap.add dt_var (List.assoc free_id subst) map )
              IntMap.empty free_vars
          in
          Some (value, subst)
      | hd :: tl, Inner node -> (
          ( match get_step hd with
          | None -> None
          | Some (symbol, children) -> (
            match IntTable.get node.steps symbol with
            | None -> None
            | Some x -> traverse (children @ tl) subst x ) )
          |> (* Ordinary lookup using the outermost feature of the stype hd failed.
             Now try to unify with the free vars, starting with the smallest.
             By doing this, we guarantee (proof?!) that ('a * 'a) is preferred
             over ('a * 'b).
             Rationale: the smallest was bound further up in the path and thus
             is the most restricting choice.
          *)
          function
          | Some x -> Some x
          | None ->
              let rec loop seq =
                match seq () with
                | Seq.Nil -> None
                | Seq.Cons ((free_id, tree), rest) -> (
                  match List.assoc_opt free_id subst with
                  | None -> traverse tl ((free_id, hd) :: subst) tree
                  | Some stype ->
                      if equal stype hd then traverse tl subst tree
                      else loop rest )
              in
              (* TODO: change type of node.free to something ordered *)
              let sorted =
                IntTable.fold
                  (fun k v acc -> IntMap.add k v acc)
                  node.free IntMap.empty
              in
              loop (IntMap.to_seq sorted) )
      | [], _ | _ :: _, Leave _ -> assert false
      (* This should be impossible. [Symbol.of_stype] should
                        uniquely identify the number of children on each step.
                     *)
    in
    traverse [stype] [] t.tree

  let add t stype value =
    let get_step =
      let modulo_props = t.modulo_props in
      Symbol.of_stype_register ~modulo_props
    in
    let rec traverse stack free_vars tree =
      match (tree, stack) with
      | Leave _, [] ->
          raise (Invalid_argument "(congruent) type already registered")
      | Inner node, hd :: tl -> (
        match get_step hd with
        | Symbol (symbol, children) -> (
            let nstack = children @ tl in
            match IntTable.get node.steps symbol with
            | None -> (
              match nstack with
              | [] -> IntTable.set node.steps symbol (Leave {value; free_vars})
              | _ ->
                  let tree = create_node () in
                  IntTable.set node.steps symbol tree ;
                  traverse nstack free_vars tree )
            | Some tree -> traverse nstack free_vars tree )
        | Var dt_var -> (
            let free_id =
              (* Was this dt_var already observed further up in the path?
                 If so, reuse free_id, else bump free_id. *)
              match List.assoc_opt dt_var free_vars with
              | Some free_id -> free_id
              | None ->
                  let last =
                    (* TODO: check first element might be enough *)
                    List.fold_left max (-1) (List.rev_map fst free_vars)
                  in
                  last + 1
            in
            let free_vars = (dt_var, free_id) :: free_vars in
            match IntTable.get node.free free_id with
            | Some tree -> traverse tl free_vars tree
            | None -> (
              match tl with
              | [] -> IntTable.set node.free free_id (Leave {value; free_vars})
              | _ ->
                  let tree = create_node () in
                  IntTable.set node.free free_id tree ;
                  traverse tl free_vars tree ) ) )
      | _, _ -> failwith "inconsistent tree"
    in
    traverse [stype] [] t.tree
end

let%test _ =
  let module A = Tree (Symbol ()) in
  let open A in
  let open Stype in
  let print_substitution fmt map =
    IntMap.iter
      (fun i stype ->
        Format.fprintf fmt " DT_var %i -> %a;" i Stype.print stype )
      map
  in
  let print fmt = function
    | None -> Format.fprintf fmt "None"
    | Some (i, s) -> Format.fprintf fmt "Some (%i, %a)" i print_substitution s
  in
  let table = create ~modulo_props:true in
  let tadd typ = add table (Ttype.to_stype typ) in
  let open Std in
  tadd (list_t int_t) 1 ;
  tadd (option_t string_t) 2 ;
  tadd int_t 3 ;
  add table (DT_var 0) 42 ;
  add table (DT_list (DT_var 0)) 4 ;
  add table (DT_tuple [DT_var 0; DT_var 0]) 5 ;
  add table (DT_tuple [DT_var 1; DT_var 0]) 6 ;
  (* this fails as expected *)
  (* add (DT_var 1) 42 *)
  let s = Ttype.to_stype in
  List.for_all
    (fun (stype, expected) ->
      let got = get table stype in
      if got = expected then true
      else
        let () =
          Format.printf "expected: %a\ngot: %a\n%!" print expected print got
        in
        false )
    [ (s int_t, Some (3, IntMap.empty))
    ; (s (list_t string_t), Some (4, IntMap.singleton 0 DT_string))
    ; (s (list_t int_t), Some (1, IntMap.empty))
    ; (s (option_t string_t), Some (2, IntMap.empty))
    ; ( s (list_t (array_t int_t))
      , Some (4, IntMap.singleton 0 (DT_array DT_int)) )
    ; (s [%t: int * int], Some (5, IntMap.singleton 0 DT_int))
    ; ( s [%t: int * bool]
      , Some (6, IntMap.(singleton 0 (s bool_t) |> add 1 (s int_t))) )
    ; (s [%t: int option], Some (42, IntMap.singleton 0 (DT_option DT_int))) ]

module type S = sig
  type t
  type 'a data

  val create : modulo_props:bool -> t
  (** The matcher without any registered pattern. *)

  val add : t -> t:'a Ttype.t -> 'a data -> unit
  (** Add a case to the matcher. *)

  (** {2 Match types with free variables} *)

  module type C0 = sig
    include Unify.T0

    val data : t data
  end

  val add0 : t -> (module C0) -> unit
  (** Add a case to the matcher. Equivalent to {!add}. *)

  module type C1 = sig
    include Unify.T1

    val data : 'a Ttype.t -> 'a t data
  end

  val add1 : t -> (module C1) -> unit
  (** Add a case to the matcher. One free variable.*)

  module type C2 = sig
    include Unify.T2

    val data : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t data
  end

  val add2 : t -> (module C2) -> unit
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

  val apply : t -> t:'a Ttype.t -> 'a matched option
  val apply_exn : t -> t:'a Ttype.t -> 'a matched
end

module Make (Data : sig
  type 'a t
end) : S with type 'a data = 'a Data.t = struct
  (* TODO: document, that symbol table is stored in Module, not in Matcher.t *)

  module Symbol = Symbol ()
  module Tree = Tree (Symbol)

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

  type candidate = C0 of (module C0) | C1 of (module C1) | C2 of (module C2)
  type t = candidate Tree.t

  let create ~modulo_props : t = Tree.create ~modulo_props

  let add tree (type a) ~(t : a Ttype.t) (data : a data) =
    let c = C0 (module struct type t = a

                              let t = t
                              let data = data end) in
    Tree.add tree (Ttype.to_stype t) c

  let add0 tree (module C : C0) =
    Tree.add tree (Ttype.to_stype C.t) (C0 (module C))

  type var

  let var i : var Ttype.t = Obj.magic (Stype.DT_var i)
  let v0 = var 0
  let v1 = var 1

  let add1 tree (module C : C1) =
    Tree.add tree (Ttype.to_stype (C.t v0)) (C1 (module C))

  let add2 tree (module C : C2) =
    Tree.add tree (Ttype.to_stype (C.t v0 v1)) (C2 (module C))

  let ttype : type a. int -> Tree.substitution -> a Ttype.t =
   fun i map ->
    match IntMap.find_opt i map with
    | None -> Obj.magic Std.unit_t
    (* Unification succeeded, but type variable
                                        was not used. *)
    | Some s -> Obj.magic s

  (* Unification succeeded by instantiating type
                                 variable with stype s. *)

  let[@landmark] apply : type a. t -> t:a Ttype.t -> a matched option =
   fun tree ~t ->
    let stype = Ttype.to_stype t in
    match Tree.get tree stype with
    | None -> None
    | Some (C0 (module C : C0), map) ->
        assert (IntMap.cardinal map = 0) ;
        let module M : M0 with type matched = a = struct
          include C

          type matched = a

          let eq = Obj.magic TypEq.refl
        end in
        Some (M0 (module M))
    | Some (C1 (module C : C1), map) ->
        assert (IntMap.cardinal map < 2) ;
        let module M : M1 with type matched = a = struct
          include C

          type matched = a
          type a

          let eq = Obj.magic TypEq.refl
          let data = data (ttype 0 map)
        end in
        Some (M1 (module M))
    | Some (C2 (module C : C2), map) ->
        assert (IntMap.cardinal map < 3) ;
        let module M : M2 with type matched = a = struct
          include C

          type matched = a
          type a
          type b

          let eq = Obj.magic TypEq.refl
          let data = data (ttype 0 map) (ttype 1 map)
        end in
        Some (M2 (module M))

  let apply_exn tree ~t =
    match apply tree ~t with None -> raise Not_found | Some m -> m
end
