(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** Pattern matching on dynamic types. *)

module type S = sig
  type t
  type 'a data

  val create : modulo_props:bool -> t
  (** The matcher without any registered pattern. *)

  val add : t -> t:'a Ttype.t -> 'a data -> unit
  (** Add a case to the matcher. *)

  (** {3 Match types with free variables} *)

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

  (** {3 Matching Result} *)

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

  (** {3 Executing the Matcher} *)

  val apply : t -> t:'a Ttype.t -> 'a matched option
  val apply_exn : t -> t:'a Ttype.t -> 'a matched
end

module Make (Data : sig
  type 'a t
end) : S with type 'a data = 'a Data.t
