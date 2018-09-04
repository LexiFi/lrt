(** Typed representation of types. *)

open Stype

module TypEq : sig
  (** Type equalities *)

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

type 'a ttype

(** Packed ttype and value. *)
type dynamic = Dyn: 'a ttype * 'a -> dynamic

val ttypes_equality: 'a ttype -> 'b ttype -> ('a, 'b) TypEq.t option
val ttypes_equality_modulo_props: 'a ttype -> 'b ttype
  -> ('a, 'b) TypEq.t option

val split_arrow_ttype: ('a -> 'b) ttype -> 'a ttype * 'b ttype
val build_arrow_ttype: 'a ttype -> 'b ttype -> ('a -> 'b) ttype

val ttype_fst: ('a * 'b) ttype -> 'a ttype
val ttype_snd: ('a * 'b) ttype -> 'b ttype

val remove_first_props_ttype: 'a ttype -> 'a ttype
val add_props: Stype.stype_properties -> 'a ttype -> 'a ttype

val abstract_ttype: 'a ttype -> 'a ttype


(** {2 Upgrade stype to ttype} *)

type is_ttype = Ttype: 'a ttype -> is_ttype
val ttype_of_stype: stype -> is_ttype

(** {2 Downgrade ttype to stype} *)

val stype_of_ttype: _ ttype -> stype
