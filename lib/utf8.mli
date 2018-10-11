(***************************************************************************)
(*  Copyright (C) 2000-2018 LexiFi SAS. All rights reserved.               *)
(*                                                                         *)
(*  No part of this document may be reproduced or transmitted in any       *)
(*  form or for any purpose without the express permission of LexiFi SAS.  *)
(***************************************************************************)

val of_latin1: string -> string
(** Converts a latin1 string to utf-8. *)

val to_latin1: string -> string
(** Converts a utf-8 string to latin1.  Characters which cannot be represented
    in latin1 are escaped in a special way. *)

val next: string -> int ref -> Uchar.t
(** [next s i] tries to decode a utf8 code point at position [i] in [s].  If
    successful, return the value of code point and updates [i] with the position
    of the next code point.  Otherwise, raise [Failure _]. *)

val is_valid: string -> bool
(** [is_valid s] returns whether [s] is a valid UTF-8 string. *)
