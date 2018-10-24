(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

module type S = sig
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

module Make (Unit : sig end) : S = struct
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

(* TODO: investigate why this is commented
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
