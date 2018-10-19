(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** Type equalities. *)

(** A value of type [('a, 'b) t] is a witness that the two types ['a] and ['b]
    are equal. *)
type (_, _) t = Eq : ('a, 'a) t

val refl : ('a, 'a) t
val trans : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val sym : ('a, 'b) t -> ('b, 'a) t
val app : ('a, 'b) t -> 'a -> 'b

module Lift (T : sig
  type 'a c
end) : sig
  val eq : ('a, 'b) t -> ('a T.c, 'b T.c) t
end
