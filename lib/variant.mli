(** Universal untyped representation of values.

    Values can be converted to and from variants using {!variant} and
    {!of_variant}. *)

open Dynt_core.Ttype

type t =
  | Unit
  | Bool of bool
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | Nativeint of nativeint
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

val variant: t:'a ttype -> 'a -> t
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

(* val check_variantizable: 'a ttype -> unit *)

(* exception Bad_type_for_variant of stype * t * string *)

(* val of_variant: t:'a ttype -> t -> 'a *)
(** Rebuild a typed value from a variant. May raise [Bad_type_for_variant]
    if [t] does not match the type of the variant or [Failure] if e.g. a value
    of an abstract type needs to be rebuilt but there is no registered
    de-variantizer.
*)

(* val print_variant: Format.formatter -> t -> unit *)
(** Print a variant with the syntax of MLFi constants. *)
