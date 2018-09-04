
(** {2 Type equalities} *)

module TypEq : sig
  type (_, _) t = Eq: ('a, 'a) t
  (** A value of type [('a, 'b) t] is a witness that the two types ['a] and
   * ['b] are equal. *)

  val refl: ('a, 'a) t
  val trans: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val sym: ('a, 'b) t -> ('b, 'a) t

  val app: ('a, 'b) t -> 'a -> 'b

  module Lift(T : sig type 'a c end) : sig
    val eq: ('a, 'b) t -> ('a T.c, 'b T.c) t
  end
end

(** {2 Typed type description} *)

type 'a ttype
external stype_of_ttype: _ ttype -> Stype.stype = "%identity"

val ttypes_equality: 'a ttype -> 'b ttype -> ('a, 'b) TypEq.t option
val ttypes_equality_modulo_props: 'a ttype -> 'b ttype
  -> ('a, 'b) TypEq.t option

val split_arrow_ttype: ('a -> 'b) ttype -> 'a ttype * 'b ttype
val build_arrow_ttype: 'a ttype -> 'b ttype -> ('a -> 'b) ttype

val ttype_fst: ('a * 'b) ttype -> 'a ttype
val ttype_snd: ('a * 'b) ttype -> 'b ttype

val remove_first_props_ttype: 'a ttype -> 'a ttype
val add_props: (string * string) list -> 'a ttype -> 'a ttype

val abstract_ttype: 'a ttype -> 'a ttype

(** {2 Packed ttype and value} **)

type dynamic = Dyn: 'a ttype * 'a -> dynamic
