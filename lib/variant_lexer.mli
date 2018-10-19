(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** Variant parsing *)

type t =
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

type error =
  | Unterminated_string
  | Unterminated_string_in_comment
  | Unterminated_comment
  | Illegal_escape of string
  | Literal_overflow of string
  | Illegal_date_value
  | Illegal_character of char
  | Syntax_error  (** Errors reported during variant lexing *)

(** Raised when an exception is raised by the variant parser.
        [msg] contains the error message; [text] is the fragment
        of source showing the error; [loc] is a description of the
        error location *)
exception Error of {msg: string; text: string; loc: string}

val variant_of_string : string -> t
(** Parse a textual representation of a variant (produced e.g. by
    {!Mlfi_isdatypes.string_one_line_of_variant} from a string. *)

val variant_of_file : string -> t
(** Parse a textual representation of a variant (produced e.g. by
    {!Mlfi_isdatypes.string_one_line_of_variant} from a text file. *)
