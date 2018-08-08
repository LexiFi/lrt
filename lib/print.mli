(******************************************************************************)
(*  Copyright (C) 2020 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

(** Dynamic printing. *)

val print : t:'a Ttype.t -> Format.formatter -> 'a -> unit
(** Print a value to the given formatter. *)

val show : t:'a Ttype.t -> 'a -> unit
(** Print a value to stdout. *)

(** {3 Handling abstract types} *)

type 'a printer = Format.formatter -> 'a -> unit

module type PRINTABLE_0 = sig
  include Unify.T0

  val printer : t printer
end

module type PRINTABLE_1 = sig
  include Unify.T1

  val printer : 'a printer -> 'a t printer
end

module type PRINTABLE_2 = sig
  include Unify.T2

  val printer : 'a printer -> 'b printer -> ('a, 'b) t printer
end

(** The following raise [Failure] on non-abstract types. *)

val add_abstract_0 : (module PRINTABLE_0) -> unit

val add_abstract_1 : (module PRINTABLE_1) -> unit

val add_abstract_2 : (module PRINTABLE_2) -> unit

(** {3 Unsafe printing for abstract types}

    The abstract types are matched by name only.
*)

module type UNSAFE_ABSTRACT_PRINTABLE_1 = sig
  type 'a t

  val name : string

  val printer : 'a printer -> 'a t printer
end

module type UNSAFE_ABSTRACT_PRINTABLE_2 = sig
  type ('a, 'b) t

  val name : string

  val printer : 'a printer -> 'b printer -> ('a, 'b) t printer
end

val add_unsafe_abstract_0
  :  name:string ->
  (Format.formatter -> 'a printer) ->
  unit

val add_unsafe_abstract_1 : (module UNSAFE_ABSTRACT_PRINTABLE_1) -> unit

val add_unsafe_abstract_2 : (module UNSAFE_ABSTRACT_PRINTABLE_2) -> unit
