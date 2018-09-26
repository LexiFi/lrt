(** Universal untyped representation of values.

    Values can be converted to and from variants using {!variant} and
    {!of_variant}. *)

open Dynt_core.Ttype

type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Char of char
  | Tuple of t list
  | List of t list
  | Array of t array
  | Option of t option
  | Record of (string * t) list
  | Constructor of string * t option
  | Variant of t
  | Lazy of t Lazy.t
[@@deriving t]

val to_variant: t:'a ttype -> 'a -> t
(**
   Transform a typed value into a variant.

   One may write, for instance, [variant (45, "hello")], in place of
   [V_tuple[V_int 45; V_string "hello"]].

   Note: variantizing constructors named "None" or "Some" (but not part
   of the "option" type) can result in unexpected behavior if the variant
   go through a roundtrip with textual syntax.

   May raise the [Failure] exception if the value cannnot be
   variantized.
*)

exception Bad_type_for_variant of Dynt_core.Stype.stype * t * string

val of_variant: t:'a ttype -> t -> 'a
(** Rebuild a typed value from a variant. May raise [Bad_type_for_variant]
    if [t] does not match the type of the variant or [Failure] if e.g. a value
    of an abstract type needs to be rebuilt but there is no registered
    de-variantizer.
*)

val print_variant: Format.formatter -> t -> unit
(** Print a variant with the syntax of MLFi constants. *)

(** {Handle abstract types} *)

(** This does only support abstract types *)

type 'a to_variant = t: 'a ttype -> 'a -> t
type 'a of_variant = t: 'a ttype -> t -> 'a

module type VARIANTIZABLE_0 = sig
  include Xtype.TYPE_0
  val to_variant: t to_variant
  val of_variant: t of_variant
end

module type VARIANTIZABLE_1 = sig
  include Xtype.TYPE_1
  val to_variant: 'a t to_variant
  val of_variant: 'a t of_variant
end

module type VARIANTIZABLE_2 = sig
  include Xtype.TYPE_2
  val to_variant: ('a, 'b) t to_variant
  val of_variant: ('a, 'b) t of_variant
end

val add_abstract_0: (module VARIANTIZABLE_0) -> unit
val add_abstract_1: (module VARIANTIZABLE_1) -> unit
val add_abstract_2: (module VARIANTIZABLE_2) -> unit
