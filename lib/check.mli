(***************************************************************************)
(*  Copyright (C) 2000-2018 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

open Dynt_core

(** A quickcheck-like library for LexiFi compiler. *)

(** {2 Generators} *)

type 'a gen
(** Generators of ['a] values. *)

val return: 'a -> 'a gen
(** Create a generator that always generates the given value. *)

val join: 'a gen gen -> 'a gen

val map: ('a -> 'b) -> 'a gen -> 'b gen
(** Map a function over a generator. *)

val map2: ('a -> 'b -> 'c) -> 'a gen -> 'b gen -> 'c gen

val (<$>): ('a -> 'b) -> 'a gen -> 'b gen
(** Infix for [map]. *)

val (<*>): ('a -> 'b) gen -> 'a gen -> 'b gen
(** Sequential applicaton.

   Can be used together with [<$>] to allow programming in an applicative
   style while remaining in the world of generators. E.g:

   If [f] has type ['a -> 'b -> 'c -> 'd] then [f <$> g1 <*> g2 <*> g3]
   generates three values using the generators [g1], [g2] and [g3], and then
   applies [f] to them, yielding a new generator of type ['d gen].
*)

val (>>=): 'a gen -> ('a -> 'b gen) -> 'b gen
(** Sequentially compose two generators, passing the value produced by the
    first as an argument to the second. *)

val sized: (int -> 'a gen) -> 'a gen

val try_with: 'a gen -> (exn -> 'a gen) -> 'a gen

val list_sequence: 'a gen list -> 'a list gen
(** Use the given generators in sequence to produce a list of values. *)

val choose_int: int * int -> int gen
(** Generate a random integer in the given inclusive range. *)

val choose_float: float * float -> float gen
(** Generate a random float in the given inclusive range. *)

val choose_char: int * int -> char gen
(** Generate a random char in the given inclusive range. *)

val oneof: 'a gen list -> 'a gen
(** Randomly use one of the given generators. The input list must be non-empty. *)

val oneof_freq: (int * 'a gen) list -> 'a gen
(** Choose one of the given generators, with a weighted random distribution.
    The input list must be non-empty. *)

val oneof_freq_lazy: (int * 'a gen Lazy.t) list -> 'a gen
(** Like [oneof_freq] but takes lazy generators, which can be used to avoid
    infinite loops when writing recursive generators. *)

val oneof_lazy: 'a gen Lazy.t list -> 'a gen

val elements_sized: 'a list -> int -> 'a gen
val elements: 'a list -> 'a gen
(** Generate one of the given values. The input list must be non-empty. *)

val elements_freq: (int * 'a) list -> 'a gen

val elements_freq_lazy: (int * 'a Lazy.t) list -> 'a gen

val until: ('a -> bool) -> 'a gen -> 'a gen
(** Generate values until the given condition is satisfied. *)

(** {2 Generators of basic types} *)

val unit: unit gen
(** Generate [unit]. *)

val bool: bool gen
(** Generate a random [bool]. *)

val int: int gen
(** Generate a random [int]. *)

val float: float gen
(** Generate a random [float]. *)

val char: char gen
(** Generate a random [char]. *)

val lowercase_letter: char gen
(** Generate a lowercase letter [char]. *)

val uppercase_letter: char gen
(** Generate an uppercase letter [char]. *)

val digit: char gen
(** Generate a digit [char]. *)

val string: string gen
(** Generate a random [string]. *)

val string_of_size: int -> char gen -> string gen
(** Generate a random [string] of at most a given size using given [char] generator. *)

val option: 'a gen -> 'a option gen
(** Generate a random [option]. *)

val tuple: 'a gen -> 'b gen -> ('a * 'b) gen
(** Generate a tuple using the two given generators. *)

val tuple3: 'a gen -> 'b gen -> 'c gen -> ('a * 'b * 'c) gen
(** Generate a triple using the three given generators. *)

val list: 'a gen -> 'a list gen
(** Generate a list where each element is generated using the given generator. *)

val list_copy: int -> 'a gen -> 'a list gen
(** Generate a list of a given size, where each element is generated
    using the given generator. *)

val list_of_size: int -> 'a gen -> 'a list gen
(** Generate a list of at most a given size, where each element is
    generated using the given generator. *)

val array: 'a gen -> 'a array gen
(** Generate an array where each element is generated using the given generator. *)

val array_copy: int -> 'a gen -> 'a array gen
(** Generate an array of a given size, where each element is generated
    using the given generator. *)

val array_of_size: int -> 'a gen -> 'a array gen
(** Generate an array of at most a given size, where each element is
    generated using the given generator. *)

(** {2 Mlfi-specific generators} *)

val lident: string gen
(** Generate a lowercase identifier, usable as a record field name (not an MLFi keyword) *)

val uident: string gen
(** Generate an uppercase identifier, usable as a constructor name *)

module UGen: sig
  type t
  val create: t:'a ttype -> (int -> 'a gen) -> t
end

val of_type_gen_sized: UGen.t list -> t:'a ttype -> int -> 'a gen
val of_type_gen: UGen.t list -> t:'a ttype -> 'a gen
(** Generate a random value of type ['a].

    Useful for automatically creating generators for simple types, but may loop
    on recursive types or be too random for your use case.

    Can be customized with a list of custom generators.
*)

val stype: stype gen
(** Generate a random [stype]. *)

type 'a shrink = 'a -> 'a list

(** {2 Combinators to make writing properties easier} *)

val (=>): bool -> bool -> bool

val (==>): bool -> (unit -> bool) -> bool
(* Like [(=>)] but with a lazy second argument. *)

(** {2 Running the generators} *)

type 'a test_result =
  | Succeed of {name: string option; test_run: int}
  | Fail of {name: string option; test_run: int; seed: int;
             test_case: 'a; shrink_count: int option}
  | Throw of {name: string option; test_run: int; seed: int;
              test_case: 'a; shrink_count: int option; backtrace: string}

val test:
  int ->
  seed:int ->
  ?name:string ->
  generator:'a gen ->
  ?depthmax: int ->
  ?shrink:'a shrink ->
  ('a -> bool) ->
  'a test_result
(** [test n ~generator prop] checks the property [prop] [n] times by feeding
    it input from [generator]. If [seed] is specified, it is used to initialize the
    random number generator, otherwise it is left uninitialized and the behaviour
    is undeterministic. If [shrink] is specified, it should produce a (possibly
    empty) list of immediate "shrinks" of the given value. It is used to
    successively shrink values that falsifies the property until a "minimal" value
    that falsfies the property is found. We don't yet provide exact definitions of
    "shrinks" and "minimal". The function returns structured data to be used in external test runners.
*)

(** {2 Shrinking} *)

module Shrink:
sig
  (* Basic types. *)

  val bool: bool shrink

  val char: char shrink

  val int: int shrink

  val float: float shrink

  val string: string shrink

  val option: 'a shrink -> 'a option shrink

  val tuple: 'a shrink -> 'b shrink -> ('a * 'b) shrink

  val tuple3: 'a shrink -> 'b shrink -> 'c shrink -> ('a * 'b * 'c) shrink

  val tuple4: 'a shrink -> 'b shrink -> 'c shrink -> 'd shrink -> ('a * 'b * 'c * 'd) shrink

  val tuple5: 'a shrink -> 'b shrink -> 'c shrink -> 'd shrink -> 'e shrink -> ('a * 'b * 'c * 'd * 'e) shrink

  val tuple6: 'a shrink -> 'b shrink -> 'c shrink -> 'd shrink -> 'e shrink -> 'f shrink -> ('a * 'b * 'c * 'd * 'e * 'f) shrink

  val tuple7: 'a shrink -> 'b shrink -> 'c shrink -> 'd shrink -> 'e shrink -> 'f shrink -> 'g shrink -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) shrink

  val list: 'a shrink -> 'a list shrink

  val list2: 'a shrink -> 'a list shrink

  val array: 'a shrink -> 'a array shrink

  (* MLFi types. *)

  val of_type: t:'a ttype -> 'a shrink
end
