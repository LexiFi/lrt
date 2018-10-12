(** Pattern matching on dynamic types. *)

module type CASE = sig
  type 'a payload
end

module Make : functor (C: CASE) -> sig
  type t

  val empty: modulo_props:bool -> t
  (** The matcher without any registered pattern. *)

  val add: t: 'a Ttype.t -> 'a C.payload -> t -> t
  (** Add a case to the matcher. *)

  (** {2 Match types with free variables} *)

  module type C0 = sig
    include Unify.T0
    val payload : t C.payload
  end

  val add0: (module C0) -> t -> t
  (** Add a case to the matcher. Equivalent to {!add}. *)

  module type C1 = sig
    include Unify.T1
    val payload : 'a Ttype.t -> 'a t C.payload
  end

  val add1: (module C1) -> t -> t
  (** Add a case to the matcher. One free variable.*)

  module type C2 = sig
    include Unify.T2
    val payload : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t C.payload
  end

  val add2: (module C2) -> t -> t
  (** Add a case to the matcher. Two free variables. *)

  (** {2 Executing the Matcher} *)

  module type M0 = sig
    include C0
    type matched
    val eq : (t, matched) TypEq.t
  end

  module type M1 = sig
    include C1
    type matched
    type a [@@deriving t]
    val eq : (a t, matched) TypEq.t
  end

  module type M2 = sig
    include C2
    type matched
    type a [@@deriving t]
    type b [@@deriving t]
    val eq : ((a, b) t, matched) TypEq.t
  end

  type 'a matched =
    | U0 of (module M0 with type matched = 'a)
    | U1 of (module M1 with type matched = 'a)
    | U2 of (module M2 with type matched = 'a)


  val apply: t -> t: 'a Ttype.t -> 'a matched option
end
