(** Access deeply nested values *)

type (_, _) t =
  | (::) : ('a, 'b) step * ('b, 'c) t -> ('a, 'c) t
  | [] : ('a, 'a) t

and ('a, 'b) step = ('a, 'b) lens * meta

and ('a, 'b) lens =
  { get : 'a -> 'b option
  ; set : 'a -> 'b -> 'a option
  }

and meta = private
  | Field of { name : string }
  | Constructor of { name : string; arg : constructor_argument }
  | Tuple of { nth : int; arity : int }
  | List of { nth : int }
  | Array of { nth : int }

and constructor_argument =
  | Regular of { nth : int; arity : int }
  | Inline of { field : string }

val meta_list : ('a, 'b) t -> meta list
(** Read the abstract representation from a path *)

val print : Format.formatter -> ('a, 'b) t -> unit
(** Print a path in the syntax expected by the syntax extension *)

val lens : ('a, 'b) t -> ('a, 'b) lens
(** Condense a path to a single lens *)

(** / **)

module Unsafe : sig
  (** Constructors for the private type [meta]. This should not be used directly
      because the rest of the module assumes consistency between the lens and
      meta information. *)

  val field: name:string -> meta
  val tuple: nth:int -> arity:int -> meta
  val constructor_regular: name:string -> nth:int -> arity:int -> meta
  val constructor_inline: name:string -> field:string -> meta
  val list: nth:int -> meta
  val array: nth:int -> meta
end [@@ocaml.deprecated "Do not use this module directly"]

