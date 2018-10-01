(** Typed runtime representation of types. *)

type 'a t

val print : Format.formatter -> 'a t -> unit

(** A dynamically typed value. *)
type dynamic = Dyn: 'a t * 'a -> dynamic

val equality: 'a t -> 'b t -> ('a, 'b) TypEq.t option
val equality_modulo_props: 'a t-> 'b t -> ('a, 'b) TypEq.t option

val split_arrow: ('a -> 'b) t-> 'a t * 'b t
val build_arrow: 'a t -> 'b t -> ('a -> 'b) t

val fst: ('a * 'b) t -> 'a t
val snd: ('a * 'b) t -> 'b t

val remove_outer_props: 'a t -> 'a t
(** Remove properties from a ttype. If properties are nested, all are removed.*)

val consume_outer_props: 'a t -> Stype.properties * 'a t
(** Read the properties from a ttype and returns a ttype that is not a property
    node. If properties are nested, the innermost properties are at the
    beginning of the return list.

    In the sense of this function the following types [s] and [t] carry the same
    list of properties.
    {[
type s' = int [@prop {a = "b"; b = "c"}] [@@deriving t]
type s = s' [@prop {c = "d"; d = "e"}] [@@deriving t]
type t = int [@prop {a = "b"; b = "c"; c = "d"; d = "e"}] [@@deriving t]
assert (fst (consume_outer_props s_t) = fst (consume_outer_props t))
    ]}
*)

val add_props: Stype.properties -> 'a t -> 'a t

(** {2 Conversion to/from stype} *)

type is_t = Ttype: 'a t -> is_t
val of_stype: Stype.t -> is_t

val to_stype: _ t -> Stype.t
