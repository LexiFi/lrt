(** Pattern matching on dynamic types. *)

module type S = sig
  type t
  type 'a data

  val empty: modulo_props:bool -> t
  (** The matcher without any registered pattern. *)

  val add: t: 'a Ttype.t -> 'a data -> t -> t
  (** Add a case to the matcher. *)

  (** {2 Match types with free variables} *)

  module type C0 = sig
    include Unify.T0
    val data : t data
  end

  val add0: (module C0) -> t -> t
  (** Add a case to the matcher. Equivalent to {!add}. *)

  module type C1 = sig
    include Unify.T1
    val data : 'a Ttype.t -> 'a t data
  end

  val add1: (module C1) -> t -> t
  (** Add a case to the matcher. One free variable.*)

  module type C2 = sig
    include Unify.T2
    val data : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t data
  end

  val add2: (module C2) -> t -> t
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

  val apply: t -> t: 'a Ttype.t -> 'a matched option

  val apply_exn: t -> t: 'a Ttype.t -> 'a matched
  (** raise Not_found *)

end

module Make (Data: sig type 'a t end) : S with type 'a data = 'a Data.t
