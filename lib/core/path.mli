(** Access deeply nested values.

    This module introduces paths within types and values.

    Paths are constructed using the syntax extension [\[%path? P\]].
    The payload P is a list of steps, each following this syntax:

      - [ ([],_,_) ] to access the first element of a triple.
      - [ f ] to access the record field [f].
      - [ C (_,[]) ] to access the second argument to variant constructor [C].
      - [ C f ] to access inline field [f] of variant constructor [C].
      - [ [7] ] to access the seventh element of a list.
      - [ [|5|] ] to access the fifth element of an array.

    The empty path can be written [\[%path? []\]] or [Path.\[\]].

    Examples:

      - [\[%path? \[f; (\[\],_)\]\]] corresponds to [fun x -> fst x.f]
      - [\[%path? \[C f; \[|1|\]\]\]] corresponds to
        [function C x -> Array.get x.f 1]
*)

type (_, _) t =
  | (::) : ('a, 'b) step * ('b, 'c) t -> ('a, 'c) t
  | [] : ('a, 'a) t
  | Composed : ('a,'b) t * ('b,'c) t -> ('a, 'c) t

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

val ( @ ): ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** [a @ b] is equivalent to [Composed (a, b)]. *)

val meta_list : ('a, 'b) t -> meta list
(** Read the abstract representation from a path. *)

val print : Format.formatter -> ('a, 'b) t -> unit
(** Print a path in the syntax expected by the syntax extension. *)

val lens : ('a, 'b) t -> ('a, 'b) lens
(** Condense a path to a single lens. *)

module Unsafe : sig
  (** Operations based on the untyped meta information.

      The following functions compare paths based on the untyped meta
      information. This meta information is derived from an untyped
      representation of your program using a PPX. You are able to
      construct distinct paths with the same meta information.
      This module will interpret such paths as equal. *)

  val is_equal: ('a, 'b) t -> ('a, 'b) t -> bool
  (** [is_equal path1 path2] checks if [path1] and [path2] consist of the same
      steps. *)

  val is_prefix: ('a, 'b) t -> ('a, 'c) t -> ('b, 'c) t option
  (** [is_prefix path1 path2] checks if [path2] starts with [path1].
      When this is the case, the function returns the remaining path. *)
end

(**/**)

module Internal : sig
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

