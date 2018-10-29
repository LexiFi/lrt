(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

module Id : sig
  (** Map stype to an identifying integer.

      TODO: Extend stype with an integer symbol identifier to avoid all
      overhead.

      TODO: can property names and rec_name/abstract names collide?
  *)

  type registry

  val empty : registry

  val range : registry -> int * int
  (** The inclusive range of used ids. *)

  type maybe_free =
    | Symbol of int * Stype.t list  (** symbol, arguments *)
    | Var of int  (** DT_var *)

  val register_stype 
    : registry -> modulo_props:bool -> Stype.t -> maybe_free * registry
  (** Identify stype. Generates new id for unkown stypes and adds it to the
      registry. *)

  val identify_stype 
    : registry -> modulo_props:bool -> Stype.t -> maybe_free option
  (** See {!register_stype}. Returns [None] if stype is unknown. *)
end = struct
  (* Strings are mapped to non-negative integers using a string table. During
     registration, we use map data structure. For lookup, we compile a string
     table.

     It is crucial to preserve the ids when going from map to table.
  *)

  module StrMap = Map.Make (String)
  module Tbl = Ext.String.Tbl

  type registry =
    { map: int StrMap.t
    ; rev_strings: string list
    ; table: Tbl.t Lazy.t
    ; min_id: int
    ; max_id: int }

  let add_name r name =
    let rev_strings = name :: r.rev_strings in
    let max_id = r.max_id + 1 in
    ( max_id
    , { rev_strings
      ; max_id
      ; map= StrMap.add name max_id r.map
      ; table= lazy (Tbl.prepare (List.rev rev_strings))
      ; min_id= r.min_id } )

  let register_name r name =
    match StrMap.find_opt name r.map with
    | Some i -> (i, r)
    | None -> add_name r name

  let lookup_name r name =
    let i = Tbl.lookup (Lazy.force r.table) name in
    if i >= 0 then Some i else None

  (* Non-strings are mapped to the negative integers. *)

  let last = ref 0
  let next () = decr last ; !last
  let int = next ()
  let float = next ()
  let string = next ()
  let array = next ()
  let list = next ()
  let option = next ()
  let arrow = next ()
  let tuple0 = next ()

  (* Disable next (), since it interferes with tuple ids. *)
  type false_

  let next (x : false_) = x
  let _ = next

  let identify_tuple r arity =
    let id = tuple0 - arity in
    if id < r.min_id then None else Some id

  let register_tuple r arity =
    let min_id = tuple0 - arity in
    (min_id, {r with min_id})

  let empty =
    { max_id= -1
    ; min_id= tuple0
    ; map= StrMap.empty
    ; rev_strings= []
    ; table= lazy (Tbl.prepare []) }

  let range r = (r.min_id, r.max_id)

  type maybe_free = Symbol of int * Stype.t list | Var of int  (** DT_var *)

  (* Associativity by enforcing single prop DT_prop *)
  let rec normalize_props t = function
    | [] -> t
    | hd :: tl -> normalize_props (Stype.DT_prop ([hd], t)) tl

  let rec identify_stype 
      : registry -> modulo_props:bool -> Stype.t -> maybe_free option =
   fun r ~modulo_props -> function
    | DT_int -> Some (Symbol (int, []))
    | DT_float -> Some (Symbol (float, []))
    | DT_string -> Some (Symbol (string, []))
    | DT_list a -> Some (Symbol (list, [a]))
    | DT_array a -> Some (Symbol (array, [a]))
    | DT_option a -> Some (Symbol (option, [a]))
    | DT_arrow (_, a, b) -> Some (Symbol (arrow, [a; b]))
    | DT_prop (_, s) when modulo_props -> identify_stype r ~modulo_props s
    | DT_prop ([], s) -> identify_stype r ~modulo_props s
    | DT_prop ([(key, _)], s) -> (
      match lookup_name r key with
      | None -> None
      | Some i -> Some (Symbol (i, [s])) )
    | DT_prop (l, s) -> identify_stype r ~modulo_props (normalize_props s l)
    | DT_tuple l -> (
      match identify_tuple r (List.length l) with
      | Some id -> Some (Symbol (id, l))
      | None -> None )
    | DT_abstract (rec_name, rec_args) | DT_node {rec_name; rec_args; _} -> (
      match lookup_name r rec_name with
      | None -> None
      | Some i -> Some (Symbol (i, rec_args)) )
    | DT_object _ -> failwith "object not supported"
    | DT_var i -> Some (Var i)

  let rec register_stype 
      : registry -> modulo_props:bool -> Stype.t -> maybe_free * registry =
   fun r ~modulo_props -> function
    | DT_int -> (Symbol (int, []), r)
    | DT_float -> (Symbol (float, []), r)
    | DT_string -> (Symbol (string, []), r)
    | DT_list a -> (Symbol (list, [a]), r)
    | DT_array a -> (Symbol (array, [a]), r)
    | DT_option a -> (Symbol (option, [a]), r)
    | DT_arrow (_, a, b) -> (Symbol (arrow, [a; b]), r)
    | DT_prop (_, s) when modulo_props -> register_stype r ~modulo_props s
    | DT_prop ([], s) -> register_stype r ~modulo_props s
    | DT_prop ([(key, _)], s) ->
        let id, r = register_name r key in
        (Symbol (id, [s]), r)
    | DT_prop (l, s) -> register_stype r ~modulo_props (normalize_props s l)
    | DT_tuple l ->
        let id, r = register_tuple r (List.length l) in
        (Symbol (id, l), r)
    | DT_abstract (rec_name, rec_args) | DT_node {rec_name; rec_args; _} ->
        let id, r = register_name r rec_name in
        (Symbol (id, rec_args), r)
    | DT_object _ -> failwith "object not supported"
    | DT_var i -> (Var i, r)

  (* TODO: Runner fails with: Trying to run an expect test from the wrong file

  let%expect_test _ =
    let open Stype in
    let print = function
      | Var i -> Printf.printf "Var %i\n%!" i
      | Symbol (i, _) -> Printf.printf "Symbol %i\n%!" i
    and modulo_props = true in
    let print' = function
      | None -> Printf.printf "not registered\n%!"
      | Some x -> print x
    in
    let r = create () in
    let r' = create () in
    print (of_stype_register r ~modulo_props DT_int) ;
    print (of_stype_register r ~modulo_props (DT_array DT_int)) ;
    print (of_stype_register r ~modulo_props (DT_abstract ("a", []))) ;
    print (of_stype_register r ~modulo_props (DT_abstract ("c", []))) ;
    print (of_stype_register r ~modulo_props (DT_abstract ("d", []))) ;
    print' (of_stype r ~modulo_props DT_int) ;
    print' (of_stype r ~modulo_props (DT_tuple [DT_int; DT_int])) ;
    print' (of_stype r ~modulo_props (DT_tuple [])) ;
    print' (of_stype r ~modulo_props (DT_abstract ("a", []))) ;
    print' (of_stype r ~modulo_props (DT_abstract ("b", []))) ;
    print' (of_stype r ~modulo_props (DT_abstract ("c", []))) ;
    print' (of_stype r ~modulo_props (DT_abstract ("d", []))) ;
    print (of_stype_register r ~modulo_props (DT_abstract ("b", []))) ;
    print' (of_stype r ~modulo_props (DT_abstract ("b", []))) ;
    let print = function
      | Var i -> Printf.printf "Var %i\n%!" i
      | Symbol (i, _) -> Printf.printf "Symbol %i\n%!" i
    and modulo_props = true in
    let print' = function
      | None -> Printf.printf "not registered\n%!"
      | Some x -> print x
    in
    print' (of_stype r' ~modulo_props (DT_abstract ("b", []))) ;
    [%expect
      {|
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
end

module IntMap = Ext.Int.Map

module Tree : sig
  type 'a t
  type key = Stype.t
  type substitution = Stype.t IntMap.t

  val empty : modulo_props:bool -> 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val get : 'a t -> key -> ('a * substitution) option
end = struct
  (* On each level of the discrimination tree, we discriminate on the
     on the first symbol on the stack. Arguments returned by [Id.of_stype]
     are pushed back to the stack.

     TODO: Is it a discrimination tree? What is a discrimination tree?

     The set of paths from root to leaves in the resulting tree is homomorphic
     to the set of stypes (modulo Id.of_stype). Each path in the tree
     corresponds to one stype and vice versa.

     There are two different types of variables. On type corresponds to
     DT_vars in stypes , the other to free spots in a path. The variables
     in a path are enumerated starting from zero and constitute a normalized
     renaming of the DT_vars. We do that, such that ('a * 'b) and ('b * 'a)
     give the same path of features. During traversal of the tree, we maintain
     a mapping between the two.

     I use Id.range to make the upper level of the tree an array instead of
     a table.  Reasoning: In most cases, we do not match complex patterns, but
     only the outermost structure of an stype. E.g. we implement a new type and
     register a dynamic printer for it. In these easy cases, the overhead of a
     Hashtbl can be completely avoided.
  *)

  type key = Stype.t
  type substitution = Stype.t IntMap.t

  type 'a steps =
    { map: 'a IntMap.t (* used during insertion *)
    ; ids: Id.registry (* stypes to int mapping specific to node*)
    ; arr: 'a option array Lazy.t (* flattened map *)
    ; shift: int
    (* diff between ids in map and array indexes *) }

  type 'a tree =
    | Leave of {value: 'a; free_vars: (int * int) list}
    (* free_vars maps DT_var in stype to free vars in path *)
    | Inner of {steps: 'a tree steps; free: 'a tree IntMap.t}

  type 'a t = {modulo_props: bool; tree: 'a tree}

  let steps : type a. Id.registry -> a IntMap.t -> a steps =
   fun ids map ->
    let min, max = Id.range ids in
    let arr =
      lazy
        (let arr = Array.make (max - min + 1) None in
         IntMap.iter (fun i tree -> arr.(i - min) <- Some tree) map ;
         arr)
    in
    {map; ids; arr; shift= -min}

  let empty ~modulo_props : 'a t =
    { modulo_props
    ; tree= Inner {steps= steps Id.empty IntMap.empty; free= IntMap.empty} }

  let get t stype =
    let modulo_props = t.modulo_props in
    let equal =
      if modulo_props then Stype.equality_modulo_props else Stype.equality
    and get_step ids s =
      match Id.identify_stype ids ~modulo_props s with
      | Some (Id.Symbol (s, l)) -> Some (s, l)
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
          let ordinary =
            match get_step node.steps.ids hd with
            | None -> None
            | Some (symbol, children) -> (
              match
                (Lazy.force node.steps.arr).(symbol + node.steps.shift)
              with
              | Some x -> traverse (children @ tl) subst x
              | None -> None )
          in
          match ordinary with
          | Some x -> Some x
          | None ->
              (* Ordinary lookup using the outermost feature of the stype hd failed.
               Now try to unify with the free vars, starting with the smallest.
               By doing this, we guarantee that ('a * 'a) is preferred
               over ('a * 'b).
               Rationale: the smallest id was bound further up in the path and thus
               is the most restricting choice.
            *)
              let rec loop = function
                | [] -> None
                | (free_id, tree) :: rest -> (
                  match List.assoc_opt free_id subst with
                  | None -> traverse tl ((free_id, hd) :: subst) tree
                  | Some stype ->
                      if equal stype hd then traverse tl subst tree
                      else loop rest )
              in
              loop (IntMap.bindings node.free) )
      | [], _ | _ :: _, Leave _ -> failwith "inconsistent matcher index"
      (* This should be impossible. [Id.identify] should uniquely identify the
         number of children on each step. *)
    in
    traverse [stype] [] t.tree

  let add : type a. key -> a -> a t -> a t =
   fun stype value t ->
    let empty_tree : a tree =
      Inner {steps= steps Id.empty IntMap.empty; free= IntMap.empty}
    in
    let get_step =
      let modulo_props = t.modulo_props in
      Id.register_stype ~modulo_props
    in
    let rec traverse stack free_vars tree =
      match (tree, stack) with
      | Leave _, [] ->
          raise (Invalid_argument "(congruent) type already registered")
      | Inner node, hd :: tl -> (
        match get_step node.steps.ids hd with
        | Symbol (symbol, children), ids' -> (
            let stack' = children @ tl in
            match IntMap.find_opt symbol node.steps.map with
            | None -> (
              match stack' with
              | [] ->
                  let map' =
                    IntMap.add symbol (Leave {value; free_vars}) node.steps.map
                  in
                  Inner {node with steps= steps ids' map'}
              | _ ->
                  let tree' = traverse stack' free_vars empty_tree in
                  let map' = IntMap.add symbol tree' node.steps.map in
                  Inner {node with steps= steps ids' map'} )
            | Some tree ->
                let tree' = traverse stack' free_vars tree in
                let map' = IntMap.add symbol tree' node.steps.map in
                Inner {node with steps= steps ids' map'} )
        | Var dt_var, _ids' -> (
            let free_id, free_vars =
              (* Was this dt_var already observed further up in the path?
                   If so, reuse free_id, else bump free_id. *)
              match List.assoc_opt dt_var free_vars with
              | Some free_id -> (free_id, free_vars)
              | None -> (
                match free_vars with
                | [] -> (0, [(dt_var, 0)])
                | (_, last) :: _ as l -> (last + 1, (dt_var, last + 1) :: l) )
            in
            match IntMap.find_opt free_id node.free with
            | Some tree ->
                let tree' = traverse tl free_vars tree in
                let free' = IntMap.add free_id tree' node.free in
                Inner {node with free= free'}
            | None -> (
              match tl with
              | [] ->
                  let free' =
                    IntMap.add free_id (Leave {value; free_vars}) node.free
                  in
                  Inner {node with free= free'}
              | _ ->
                  let tree' = traverse tl free_vars empty_tree in
                  let free' = IntMap.add free_id tree' node.free in
                  Inner {node with free= free'} ) ) )
      | _, _ -> failwith "inconsistent tree"
    in
    {t with tree= traverse [stype] [] t.tree}

  let%test _ =
    let open Stype in
    let print_substitution fmt map =
      IntMap.iter
        (fun i stype ->
          Format.fprintf fmt " DT_var %i -> %a;" i Stype.print stype )
        map
    in
    let print fmt = function
      | None -> Format.fprintf fmt "None"
      | Some (i, s) ->
          Format.fprintf fmt "Some (%i, %a)" i print_substitution s
    in
    let tadd typ = add (Ttype.to_stype typ) in
    let open Std in
    let tree =
      empty ~modulo_props:true
      |> tadd (list_t int_t) 1
      |> tadd (option_t string_t) 2
      |> tadd int_t 3 |> add (DT_var 0) 42 |> add (DT_list (DT_var 0)) 4
      |> add (DT_tuple [DT_var 0; DT_var 0]) 5
      |> add (DT_tuple [DT_var 1; DT_var 0]) 6
      (* this fails as expected *)
      (* |> add (DT_var 1) 42 *)
    in
    let s = Ttype.to_stype in
    List.for_all
      (fun (stype, expected) ->
        let got = get tree stype in
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
      ; (s [%t: int option], Some (42, IntMap.singleton 0 (DT_option DT_int)))
      ]
end
