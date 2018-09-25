(***************************************************************************)
(*  Copyright (C) 2000-2018 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

(** Variant parsing *)
open Variant

type error =
  | Unterminated_string
  | Unterminated_string_in_comment
  | Unterminated_comment
  | Illegal_escape of string
  | Literal_overflow of string
  | Illegal_date_value
  | Illegal_character of char
  | Syntax_error
    (** Errors reported during variant lexing *)

exception Error of {msg:string; text:string; loc:string}
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
