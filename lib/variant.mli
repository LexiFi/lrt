(** Universal untyped representation of values.

    Values can be converted to and from variants using {!to_variant} and
    {!of_variant}. *)

type t = Variant_lexer.t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Tuple of t list
  | List of t list
  | Array of t array
  | Option of t option
  | Record of (string * t) list
  | Constructor of string * t option
  | Variant of t
  | Lazy of t Lazy.t
[@@deriving t]

val to_variant: t:'a Ttype.t -> 'a -> t
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

exception Bad_type_for_variant of Stype.t * t * string

val of_variant: t:'a Ttype.t -> t -> 'a
(** Rebuild a typed value from a variant. May raise [Bad_type_for_variant]
    if [t] does not match the type of the variant or [Failure] if e.g. a value
    of an abstract type needs to be rebuilt but there is no registered
    de-variantizer.
*)

(** {2 (De)Serialization} *)

val print_variant: Format.formatter -> t -> unit
(** Print a variant with the syntax of MLFi constants. *)

val strings_of_variant: t -> string list
(** Return a list of "string"-like components found inside the variant; useful
    to implement generic textual search. *)

val string_one_line_of_variant: t -> string
(** Return a textual representation of a variant, with the syntax
    of MLFi constants, on one line. Same behavior with respect
    to exceptions than [compact_string_of_variant]. *)

val compact_string_of_variant: ?dont_compress_records:unit ->
  ?with_more_spaces:unit -> t -> string
(** Similar to {!Mlfi_isdatypes.string_one_line_of_variant}, but use a more compact
    form, with fewer whitespaces, and a special syntax for lists and arrays of
    records (unless the [dont_compress_records] flag is used). The result is
    guaranteed to not contain any newline characters. If the variant
    contains a lazy which raises an exception, the function fails and raises
    that exception.
*)

val output_compact_string_of_variant: ?dont_compress_records:unit ->
  ?with_more_spaces:unit -> out_channel -> t -> unit
(** Same as compact_string_of_variant, but write the result into a file. *)


val variant_to_file: ?eol_lf:unit -> string -> t -> unit
(** Write a variant to a text file. *)

val value_to_variant_in_file:
  t:'a Ttype.t -> ?eol_lf:unit -> string -> 'a -> unit
(** Write a value as a variant to a text file. *)

exception Variant_parser of {msg:string; text:string; loc:string}
(** Raised when an exception is raised by the variant parser.
    [msg] contains the error message; [text] is the fragment
    of source showing the error; [loc] is a description of the
    error location *)

val variant_of_string: string -> t
(** Parse a textual representation of a variant (produced e.g. by
    {!Mlfi_isdatypes.string_one_line_of_variant} from a string. *)

val variant_of_file: string -> t
(** Parse a textual representation of a variant (produced e.g. by
    {!Mlfi_isdatypes.string_one_line_of_variant} from a text file. *)

val value_of_variant_in_file: t:'a Ttype.t -> string -> 'a
(** Read a value as a variant from a text file. *)

(** {2 Variant mapper} *)

(** Lexifi has documentation on how to use mappers and the other properties.
    This should go here *)

val of_variant_custom:
  ?name: string -> t: 'a Ttype.t -> (t -> 'a option) -> 'a Ttype.t
(** [of_variant_custom ~t custom] returns a modified [t] such that
    [of_variant ~t v] uses [custom v] as fallback when the normal
    devariantization fails.

    The optional [name] argument is used for identifying the custom
    devariantizer in error messages. (Not implemented yet.)

    Multiple custom devariantizers can be registered. They will be applied in
    order of registration.
*)

val of_variant_mapper:
  ?name: string -> t: 'a Ttype.t -> (t -> t option) -> 'a Ttype.t
(** [of_variant_mapper ~t mapper] returns a modified [t] such that
    [of_variant ~t] uses [mapper] as fallback mechanism when the normal
    conversion fails.

    This is a wrapper for [of_variant_custom].
*)

val of_variant_default:
  ?name: string -> t: 'a Ttype.t -> (unit -> 'a) -> 'a Ttype.t
(** [of_variant_default ~t init] returns a modified [t] such that
    [of_variant ~t] uses [init ()] as default value when the normal conversion
    fails.

    This is a wrapper for [of_variant_custom].
*)

(** {2 Handle abstract types} *)

type 'a to_variant = 'a -> t
type 'a of_variant = t -> 'a

(** [failwith s] raises [Bad_type_for_variant (., ., s)] with the
    corresponding stype and variant inserted. *)
type failwith = {failwith: 'a. string -> 'a} [@@unboxed]

module type VARIANTIZABLE_0 = sig
  include Xtype.T0
  val to_variant: t to_variant
  val of_variant: failwith -> t of_variant
end

module type VARIANTIZABLE_1 = sig
  include Xtype.T1
  val to_variant: 'a to_variant -> 'a t to_variant
  val of_variant: failwith -> 'a of_variant -> 'a t of_variant
end

module type VARIANTIZABLE_2 = sig
  include Xtype.T2
  val to_variant: 'a to_variant -> 'b to_variant -> ('a, 'b) t to_variant
  val of_variant: failwith -> 'a of_variant -> 'b of_variant -> ('a, 'b) t of_variant
end

(** The following raise [Failure] on non-abstract types. *)

val add_abstract_0: (module VARIANTIZABLE_0) -> unit
val add_abstract_1: (module VARIANTIZABLE_1) -> unit
val add_abstract_2: (module VARIANTIZABLE_2) -> unit
