(** Dynamic printing. *)

open Dynt_core.Ttype

val print: t:'a ttype -> Format.formatter -> 'a -> unit
val show: t:'a ttype -> 'a -> unit

(** {2 Handling abstract types} *)

type 'a printer = Format.formatter -> 'a -> unit

module type PRINTABLE_0 = sig
  include Xtype.TYPE_0
  val printer: t printer
end

module type PRINTABLE_1 = sig
  include Xtype.TYPE_1
  val printer: 'a printer -> 'a t printer
end

module type PRINTABLE_2 = sig
  include Xtype.TYPE_2
  val printer: 'a printer -> 'b printer -> ('a, 'b) t printer
end

(** The following fail on non-abstract types *)

val add_abstract_0: (module PRINTABLE_0) -> unit
val add_abstract_1: (module PRINTABLE_1) -> unit
val add_abstract_2: (module PRINTABLE_2) -> unit

(** {2 Unsafe printing for abstract types}

    The abstract types are matched by name only.
*)

module type UNSAFE_ABSTRACT_PRINTABLE_1 = sig
  type 'a t
  val name: string
  val printer: 'a printer -> 'a t printer
end

module type UNSAFE_ABSTRACT_PRINTABLE_2 = sig
  type ('a, 'b) t
  val name: string
  val printer: 'a printer -> 'b printer-> ('a, 'b) t printer
end

val add_unsafe_abstract_0:
  name: string -> (Format.formatter -> 'a printer) -> unit
val add_unsafe_abstract_1:
  (module UNSAFE_ABSTRACT_PRINTABLE_1) -> unit
val add_unsafe_abstract_2:
  (module UNSAFE_ABSTRACT_PRINTABLE_2) -> unit
