(** Dynamic printing. *)

open Dynt_core.Ttype

val print: t:'a ttype -> Format.formatter -> 'a -> unit
val show: t:'a ttype -> 'a -> unit

type 'a printer = Format.formatter -> 'a -> unit

module type ABSTRACT_PRINTABLE_0 = sig
  include Xtype.TYPE_0
  val printer: t printer
end

module type ABSTRACT_PRINTABLE_1 = sig
  include Xtype.TYPE_1
  val printer: 'a printer -> 'a t printer
end

module type ABSTRACT_PRINTABLE_2 = sig
  include Xtype.TYPE_2
  val printer: 'a printer -> 'b printer -> ('a, 'b) t printer
end

val add_abstract_type_dynamic_print_0: (module ABSTRACT_PRINTABLE_0) -> unit
val add_abstract_type_dynamic_print_1: (module ABSTRACT_PRINTABLE_1) -> unit
val add_abstract_type_dynamic_print_2: (module ABSTRACT_PRINTABLE_2) -> unit

(** { Unsafe printing for abstract types }

    The abtract types are matched by name only.
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

val add_unsafe_abstract_type_dynamic_print_0:
  name: string -> (Format.formatter -> 'a printer) -> unit
val add_unsafe_abstract_type_dynamic_print_1:
  (module UNSAFE_ABSTRACT_PRINTABLE_1) -> unit
val add_unsafe_abstract_type_dynamic_print_2:
  (module UNSAFE_ABSTRACT_PRINTABLE_2) -> unit
