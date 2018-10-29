(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.   *)
(******************************************************************************)

(** Dynamic type representations for standard ocaml types. *)

(** {3 Pervasives } *)

val unit_t : unit Ttype.t
val bool_t : bool Ttype.t
val int_t : int Ttype.t
val string_t : string Ttype.t
val float_t : float Ttype.t
val char_t : char Ttype.t
val nativeint_t : nativeint Ttype.t
val int32_t : int32 Ttype.t
val int64_t : int64 Ttype.t
val option_t : 'a Ttype.t -> 'a option Ttype.t
val list_t : 'a Ttype.t -> 'a list Ttype.t
val array_t : 'a Ttype.t -> 'a array Ttype.t

(** {3 Stdlib } *)

val lazy_t : 'a Ttype.t -> 'a Lazy.t Ttype.t
val hashtbl_t : 'a Ttype.t -> 'b Ttype.t -> ('a, 'b) Hashtbl.t Ttype.t
