(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

module Id : sig
  (** Map stype to an identifying integer.

      TODO: Extend stype with an integer symbol identifier to avoid all
      overhead.

      TODO: can property names and rec_name/abstract names collide?
  *)

  type registry

  val create : unit -> registry

  type maybe_free =
    | Symbol of int * Stype.t list  (** symbol, arguments *)
    | Var of int  (** DT_var *)

  val of_stype : registry -> modulo_props:bool -> Stype.t -> maybe_free option

  val of_stype_register 
    : registry -> modulo_props:bool -> Stype.t -> maybe_free
end = struct
  (* Strings are mapped to non-negative integers using a string table. *)

  type registry =
    {mutable strings: string list; mutable table: Ext.String.Tbl.t}

  let create () = {strings= []; table= Ext.String.Tbl.prepare []}

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
  let tuple arity = tuple0 - arity

  (* Make sure that nobody interferes with the tuples *)
  let next () = assert false
  let _ = next

  let add r name =
    r.strings <- name :: r.strings ;
    (* reverse, otherwise old strings would get new ids *)
    r.table <- Ext.String.Tbl.prepare (List.rev r.strings)

  let lookup r s =
    let i = Ext.String.Tbl.lookup r.table s in
    if i >= 0 then Some i else None

  let register r s =
    let i = Ext.String.Tbl.lookup r.table s in
    if i >= 0 then i
    else (
      add r s ;
      match lookup r s with
      | None -> failwith "broken logic in Matcher.Symbol"
      | Some i -> i )

  type maybe_free = Symbol of int * Stype.t list | Var of int  (** DT_var *)

  (* Associativity by enforcing single prop DT_prop *)
  let rec normalize_props t = function
    | [] -> t
    | hd :: tl -> normalize_props (Stype.DT_prop ([hd], t)) tl

  let rec of_stype 
      : registry -> modulo_props:bool -> Stype.t -> maybe_free option =
   fun r ~modulo_props -> function
    | DT_int -> Some (Symbol (int, []))
    | DT_float -> Some (Symbol (float, []))
    | DT_string -> Some (Symbol (string, []))
    | DT_list a -> Some (Symbol (list, [a]))
    | DT_array a -> Some (Symbol (array, [a]))
    | DT_option a -> Some (Symbol (option, [a]))
    | DT_arrow (_, a, b) -> Some (Symbol (arrow, [a; b]))
    | DT_prop (_, s) when modulo_props -> of_stype r ~modulo_props s
    | DT_prop ([], s) -> of_stype r ~modulo_props s
    | DT_prop ([(key, _)], s) -> (
      match lookup r key with None -> None | Some i -> Some (Symbol (i, [s])) )
    | DT_prop (l, s) -> of_stype r ~modulo_props (normalize_props s l)
    | DT_tuple l -> Some (Symbol (tuple (List.length l), l))
    | DT_abstract (rec_name, rec_args) | DT_node {rec_name; rec_args; _} -> (
      match lookup r rec_name with
      | None -> None
      | Some i -> Some (Symbol (i, rec_args)) )
    | DT_object _ -> failwith "object not supported"
    | DT_var i -> Some (Var i)

  let rec of_stype_register 
      : registry -> modulo_props:bool -> Stype.t -> maybe_free =
   fun r ~modulo_props -> function
    | DT_int -> Symbol (int, [])
    | DT_float -> Symbol (float, [])
    | DT_string -> Symbol (string, [])
    | DT_list a -> Symbol (list, [a])
    | DT_array a -> Symbol (array, [a])
    | DT_option a -> Symbol (option, [a])
    | DT_arrow (_, a, b) -> Symbol (arrow, [a; b])
    | DT_prop (_, s) when modulo_props -> of_stype_register r ~modulo_props s
    | DT_prop ([], s) -> of_stype_register r ~modulo_props s
    | DT_prop ([(key, _)], s) -> Symbol (register r key, [s])
    | DT_prop (l, s) -> of_stype_register r ~modulo_props (normalize_props s l)
    | DT_tuple l -> Symbol (tuple (List.length l), l)
    | DT_abstract (rec_name, rec_args) | DT_node {rec_name; rec_args; _} ->
        Symbol (register r rec_name, rec_args)
    | DT_object _ -> failwith "object not supported"
    | DT_var i -> Var i
end

let%expect_test _ =
  let open Id in
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

module IntTable = struct
  type 'a t = (int, 'a) Hashtbl.t

  open Hashtbl

  let create () = create 5
  let set = replace
  let get = find_opt
  let fold = fold
end

module IntMap = Ext.Int.Map

module Tree : sig
  type 'a t
  type key = Stype.t
  type substitution = Stype.t IntMap.t

  val create : modulo_props:bool -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val get : 'a t -> key -> ('a * substitution) option
end = struct
  (* On each level of the discrimination tree, we discriminate on the
     on the first symbol on the stack. Arguments returned by [Id.of_stype]
     are pushed back to the stack.

     The set of paths from root to leaves in the resulting tree is homomorphic
     to the set of stypes (modulo Id.of_stype). Each path in the tree
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

  type 'a t = {modulo_props: bool; tree: 'a tree; symbols: Id.registry}

  let create_node () =
    Inner {steps= IntTable.create (); free= IntTable.create ()}

  let create ~modulo_props =
    {modulo_props; tree= create_node (); symbols= Id.create ()}

  let get t stype =
    let modulo_props = t.modulo_props in
    let equal =
      if modulo_props then Stype.equality_modulo_props else Stype.equality
    and get_step s =
      match Id.of_stype t.symbols ~modulo_props s with
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
              let rec loop = function
                | [] -> None
                | (free_id, tree) :: tl' -> (
                  match List.assoc_opt free_id subst with
                  | None -> traverse tl ((free_id, hd) :: subst) tree
                  | Some stype ->
                      if equal stype hd then traverse tl subst tree
                      else loop tl' )
              in
              (* TODO: change type of node.free to something ordered *)
              let sorted =
                IntTable.fold
                  (fun k v acc -> IntMap.add k v acc)
                  node.free IntMap.empty
              in
              loop (IntMap.bindings sorted) )
      | [], _ | _ :: _, Leave _ -> assert false
      (* This should be impossible. [Symbol.of_stype] should
                        uniquely identify the number of children on each step.
      *)
    in
    traverse [stype] [] t.tree

  let add t stype value =
    let get_step =
      let modulo_props = t.modulo_props in
      Id.of_stype_register t.symbols ~modulo_props
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
  let open Tree in
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
