(** Dynamic printing. *)

open Dynt_core.Ttype

val print: t:'a ttype -> Format.formatter -> 'a -> unit
val show: t:'a ttype -> 'a -> unit

type 'a printer = 'a -> unit

module type ABSTRACT_PRINTABLE_0 = sig
  include Xtypes.TYPE_0
  val printer: Format.formatter -> t printer
end

module type ABSTRACT_PRINTABLE_1 = sig
  include Xtypes.TYPE_1
  val printer: Format.formatter -> 'a printer -> 'a t printer
end

module type ABSTRACT_PRINTABLE_2 = sig
  include Xtypes.TYPE_2
  val printer: Format.formatter -> 'a printer -> 'b printer -> ('a, 'b) t printer
end

val add_abstract_type_dynamic_print_0: (module ABSTRACT_PRINTABLE_0) -> unit
val add_abstract_type_dynamic_print_1: (module ABSTRACT_PRINTABLE_1) -> unit
val add_abstract_type_dynamic_print_2: (module ABSTRACT_PRINTABLE_2) -> unit

module type UNSAFE_ABSTRACT_PRINTABLE_1 = sig
  type 'a t
  val name: string
  val printer: Format.formatter -> 'a printer -> 'a t printer
end

module type UNSAFE_ABSTRACT_PRINTABLE_2 = sig
  type ('a, 'b) t
  val name: string
  val printer: Format.formatter -> 'a printer -> 'b printer-> ('a, 'b) t printer
end

val add_unsafe_abstract_type_dynamic_print_0:
  name: string -> (Format.formatter -> 'a printer) -> unit
val add_unsafe_abstract_type_dynamic_print_1:
  (module UNSAFE_ABSTRACT_PRINTABLE_1) -> unit
val add_unsafe_abstract_type_dynamic_print_2:
  (module UNSAFE_ABSTRACT_PRINTABLE_2) -> unit
