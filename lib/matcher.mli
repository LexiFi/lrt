(** Pattern matching on dynamic types. *)

type 'res t

val empty: modulo_props:bool -> 'res t
(** The matcher without any registered pattern. *)

val add: t: 'a Ttype.t -> f:('a -> 'res) -> 'res t -> 'res t
(** Add a case to the matcher.

    This should happen top level. The next {!apply} will cause a recompilation.
*)

val apply: 'a t -> t: 'b Ttype.t -> 'b -> 'a
(** Wrapper for {!apply'} *)

val apply': 'a t -> Ttype.dynamic -> 'a
(** [apply c (Dyn (t,x))] checks for type equality of [t] with any registered
    type that was added with [add ~t ~f].

    Returns [f x] if such a [f] is present and otherwise raises [Not_found].
*)

(** {2 Match types with free variables} *)

module type C0 = sig
  include Unify.T0
  type res
  val f: t -> res
end

val add0: (module C0 with type res = 'a) -> 'a t -> 'a t
(** Add a case to the matcher. Equivalent to {!add}. *)

module type C1 = sig
  include Unify.T1
  type res
  val f: 'a Ttype.t -> 'a t -> res
end

val add1: (module C1 with type res = 'a) -> 'a t -> 'a t
(** Add a case to the matcher. One free variable.*)

module type C2 = sig
  include Unify.T2
  type res
  val f: 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) t -> res
end

val add2: (module C2 with type res = 'a) -> 'a t -> 'a t
(** Add a case to the matcher. Two free variables. *)
