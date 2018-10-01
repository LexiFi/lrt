(** Typed representation of types. *)

type 'a t

val print : Format.formatter -> 'a t -> unit

(** Packed ttype and value. *)
type dynamic = Dyn: 'a t * 'a -> dynamic

val equality: 'a t -> 'b t -> ('a, 'b) TypEq.t option
val equality_modulo_props: 'a t-> 'b t -> ('a, 'b) TypEq.t option

val split_arrow: ('a -> 'b) t-> 'a t * 'b t
val build_arrow: 'a t -> 'b t -> ('a -> 'b) t

val fst: ('a * 'b) t -> 'a t
val snd: ('a * 'b) t -> 'b t

val remove_outer_props: 'a t -> 'a t
val add_props: Stype.properties -> 'a t -> 'a t

(** {2 Upgrade stype to ttype} *)

type is_t = Ttype: 'a t -> is_t
val of_stype: Stype.t -> is_t

(** {2 Downgrade ttype to stype} *)

val to_stype: _ t -> Stype.t
