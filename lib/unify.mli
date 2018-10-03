(** Unification of dynamic types *)

(** {2 Types with free variables} *)

module type T0 = sig
  type t [@@deriving t]
end
val t0: 'a Ttype.t -> (module T0 with type t = 'a)

module type T1 = sig
  type 'a t [@@deriving t]
end

module type T2 = sig
  type ('a, 'b) t [@@deriving t]
end

(** {2 Unification} *)

exception Not_unifiable

module U0 : functor (A: T0) (B: T0) -> sig
  include (module type of A) with type t = A.t
  type t' = B.t
  val eq : (t, t') TypEq.t
end

module U1 : functor (A: T1) (B: T0) -> sig
  include (module type of A) with type 'a t = 'a A.t
  type t' = B.t
  type a [@@deriving t]
  val eq : (a t, t') TypEq.t
end

module U2 : functor (A: T2) (B: T0) -> sig
  include (module type of A) with type ('a, 'b) t = ('a, 'b) A.t
  type t' = B.t
  type a [@@deriving t]
  type b [@@deriving t]
  val eq : ((a, b) t, t') TypEq.t
end
