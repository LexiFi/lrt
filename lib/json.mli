(***************************************************************************)
(*  Copyright (C) 2000-2018 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(** {2 Representation of JSON trees} *)

type number = I of int | F of float

type value =
  | Null
  | Bool of bool
  | Number of number
  | String of string
  | Array of value list
  | Object of (string * value) list

(** Notes:
    - The [String] payload is either Latin1-encoded or utf8-encoded,
      depending on the [utf8] flag passed to [encode/decode].
      When not in utf8 mode, a code point outside the range of that character set will
      be encoded in a special (undocumented) way.
*)


(** {2 Mapping between JSON trees and their textual representation.} *)

val encode: ?utf8:bool -> value -> string
(** Encode JSON tree into a compact JSON text (single line,  etc). *)

val decode: ?utf8:bool -> ?filename:string -> string -> value
(** Parse a JSON text into JSON tree.  Report parse errors using
    the [Failure] exception. The optional [filename] argument
    is used to report locations in error messages. *)

val to_pretty_string: value -> string
(** Prints a JSON tree into human-friendly text (multi-line, indentation).
    The output is NOT a parsable json string.
*)

type ctx
(** Context storing serialization variations directives.

    Variations:
    - to_json_field: how to translate a json field name to an mlfi record field.
*)

val ctx:
  ?to_json_field:(string -> string) ->
  unit ->
  ctx

(** {2 Typeful generic mapping between JSON trees and OCaml values.} *)

val to_json: ?ctx:ctx -> t:'a Ttype.t -> 'a -> value
(**
   [to_json x] maps an OCaml value to a JSON tree representing the same
   information.  The mapping is driven by the type of [x] and the
   default behavior (which can be overridden) is defined below.

   Basic types:
   - 1                 ---> 1
   - 1.                ---> 1.
   - ()                ---> {}
   - true/false        ---> true/false
   - "abc"             ---> "abc"
   - 2001-01-01        ---> "2001-01-01"

   List/array/tuple types:
   - [x; y]            ---> [x', y']
   - [|x; y|]          ---> [x', y']
   - (x, y)            ---> [x', y']

   Record types:
   - {l1 = x; l2 = y}    ---> {"l1": x', "l2": y'}
   - {l1 = x; l2 = None} ---> {"l1": x'}

   Sum types:
   - A                   ---> {"type": "A"}
   - B x                 ---> {"type": "B", "val": [x']}
   - C (x, y)            ---> {"type": "C", "val": [x', y']}
   - D {l1 = x; l2 = y}  ---> {"type": "D", "l1": x', "l2": y'}

   Option types:
   - Some x            ---> x'
   - None              ---> null (when not in record)

   Nested option types!
   - Some (Some x)    ---> {"type": "Some", "val": x'}
   - Some None        ---> {"type": "Some"}
   - None             ---> {"type": "None"}

   Lazy types:
   - lazy x            ---> x'
     (x is forced upon jsonification; de-jsonification is lazy)

   String Maps ('a Mlfi_sets_maps.StringMap.t)
     are mapped to objects

     e.g.
     {"a" -> 1; "b" -> 2}                 ---> {"a": 1, "b": 2}
     {"foo" -> "hello"; "bar" -> "world"} ---> {"foo": "hello", "bar": "world"}

   Special cases:
   - (x : Mlfi_isdatypes.variant) ---->  "<textual representation of x, in OCaml syntax>"
   - (x : Mlfi_json.value)        ---->  x


   Notes:

   - Function types and object types are not supported.

   - Upon parsing, extra fields in objects are accepted and ignored
     (including when parsing a sum type or unit).

   - Special float values (nan, infinity) are not supported (but
     not explicitly checked).

   - TODO?: special case when all constructors are empty (mapped to strings).

   - TODO: support some type properties (default value, etc).

*)

val of_json: ?ctx:ctx -> t:'a Ttype.t -> value -> 'a
(** Reverse mapping.  If [to_json ~t x] succeeds, the property
    [of_json ~t (to_json ~t x) = x] is expected to hold
    (except corner cases such as unchecked special float values,
    and assuming that custom converters behaves properly).
*)


(** {2 Custom mapping for specific types} *)

(* TODO: support passing local custom conversions to to_json/of_json. *)

val register_custom: t:'a Ttype.t ->
  to_json:(?ctx:ctx -> 'a -> value) ->
  of_json:(?ctx:ctx -> value -> 'a) -> unit

(** [register_conversion] registers a global custom mapping between
    OCaml values and JSON trees for a specific closed *abstract* type.

    TODO: Bring this documentation up to date. Especially, we support
    non-abstract things now.

    It is not allowed to use [null] in the JSON representation of
    values, at least if the type is used under the option type
    constructor ([null] is reserved for representing the
    [None] case).
*)

type 'a custom_json =
  { to_json: ?ctx:ctx -> 'a -> value
  ; of_json: ?ctx:ctx -> value -> 'a
  }

module Matcher : Matcher.S with type 'a data := 'a custom_json

val register_custom_0: (module Matcher.C0) -> unit

val register_custom_1: (module Matcher.C1) -> unit

val register_custom_2: (module Matcher.C2) -> unit

(* TODO: move somewhere else *)
val of_get_params: ?utf8:bool -> (string * string) list -> value
val to_get_params: ?utf8:bool -> value -> (string * string) list

module Access: sig
  val is_null: value -> bool
  val to_string: value -> string
  val to_int: value -> int
  val to_float: value -> float
  val to_list: value -> value list
  val get_field: string -> value -> value
  val string_field: string -> value -> string
  val int_field: string -> value -> int
  val float_field: string -> value -> float
  val list_field: string -> value -> value list
  val object_field: string -> value -> value
end
