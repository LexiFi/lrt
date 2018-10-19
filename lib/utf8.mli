(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the dynt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

val of_latin1 : string -> string
(** Converts a latin1 string to utf-8. *)

val to_latin1 : string -> string
(** Converts a utf-8 string to latin1.  Characters which cannot be represented
    in latin1 are escaped in a special way. *)

val next : string -> int ref -> Uchar.t
(** [next s i] tries to decode a utf8 code point at position [i] in [s].  If
    successful, return the value of code point and updates [i] with the position
    of the next code point.  Otherwise, raise [Failure _]. *)

val is_valid : string -> bool
(** [is_valid s] returns whether [s] is a valid UTF-8 string. *)
