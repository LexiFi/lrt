(** Unification of dynamic types. *)

(** {2 Types with free variables} *)

module type T0 = sig
  type t [@@deriving t]
end

val t0 : 'a Ttype.t -> (module T0 with type t = 'a)

module type T1 = sig
  type 'a t [@@deriving t]
end

module type T2 = sig
  type ('a, 'b) t [@@deriving t]
end

(** {2 Unification} *)

module type PARAM = sig
  val modulo_props : bool
end

(* TODO: might be part of the second argument (B: T0) *)

val init : modulo_props:bool -> (module PARAM)
(** The unification algorithm can be parametrized. Currently, the only parameter
    is [modulo_props]. It allows the user to specify whether properties are
    ignored or interpreted as distinguishing feature of the types. *)

(** Is raised by the following functors whenever unification is not possible. *)
exception Not_unifiable

module U0 (P : PARAM) (A : T0) (B : T0) : sig
  include T0 with type t = A.t

  type t' = B.t

  val eq : (t, t') TypEq.t
end

module U1 (P : PARAM) (A : T1) (B : T0) : sig
  include T1 with type 'a t = 'a A.t

  type t' = B.t
  type a [@@deriving t]

  (** When [P.modulo_props] is true, we cannot guarantee that [a_t] carries the
      expected properties. *)

  val eq : (a t, t') TypEq.t
end

module U2 (P : PARAM) (A : T2) (B : T0) : sig
  include T2 with type ('a, 'b) t = ('a, 'b) A.t

  type t' = B.t
  type a [@@deriving t]
  type b [@@deriving t]

  (** When [P.modulo_props] is true, we cannot guarantee that [a_t] and [b_t]
      carry the expected properties. *)

  val eq : ((a, b) t, t') TypEq.t
end
