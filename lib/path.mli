(** Access deeply nested types and values.

    This module introduces paths within types and values.

    Paths are constructed using the syntax extension [\[%path? P\]].
    The payload [P] is a list of steps, each following this syntax:

      - [ ([],_,_) ] to access the first element of a triple.
      - [ fld ] to access the record field [fld].
      - [ Cst (_,[]) ] to access the second argument to constructor [Cst].
      - [ Cst fld ] to access inline record field [fld] of constructor [Cst].
      - [ [7] ] to access the seventh element of a list.
      - [ [|5|] ] to access the fifth element of an array.

    The empty path can be written [\[%path? []\]] or [Path.\[\]].

    Examples:

      - [\[%path? \[fld; (\[\],_)\]\]] corresponds to [fun x -> fst x.fld]
      - [\[%path? \[Cst fld; \[|1|\]\]\]] corresponds to
        [fun (Cst x) -> Array.get x.fld 1]
*)

type (_, _) t =
  | ( :: ) : ('a, 'b) step * ('b, 'c) t -> ('a, 'c) t
  | [] : ('a, 'a) t

and ('a, 'b) step = ('a, 'b) lens * meta

and ('a, 'b) lens = {get: 'a -> 'b option; set: 'a -> 'b -> 'a option}

and meta = private
  | Field of {field_name: string}
  | Constructor of {name: string; arg: argument}
  | Tuple of {nth: int; arity: int}
  | List of {nth: int}
  | Array of {nth: int}

and argument =
  | Regular of {nth: int; arity: int}
  | Inline of {field_name: string}

val ( @ ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** [a @ b] composes the paths [a] and [b]. *)

val lens : ('a, 'b) t -> ('a, 'b) lens
(** Condense a path to a single lens. *)

val meta : ('a, 'b) t -> meta list
(** Reads the unsafe representation of steps from a path. *)

val print : Format.formatter -> ('a, 'b) t -> unit
(** Print a path in the syntax expected by the syntax extension. *)

module Unsafe : sig
  (** The following functions compare paths based on the untyped meta
      information. This meta information is derived from an untyped
      representation of your program using a PPX. You might be able to construct
      distinct paths with the same meta information.  This module will interpret
      such paths as equal and potentially produce unexpected results. *)

  val is_equal : ('a, 'b) t -> ('a, 'b) t -> bool
  (** [is_equal path1 path2] checks if [path1] and [path2] consist of the same
      steps. *)

  val is_prefix : ('a, 'b) t -> ('a, 'c) t -> ('b, 'c) t option
  (** [is_prefix path1 path2] checks if [path2] starts with [path1].
      When this is the case, the function returns the remaining path. *)
end
(** Operations based on the untyped meta information. *)

(**/**)

module Internal : sig
  (** Constructors for the private type [meta]. This should not be used directly
      because other parts of the module assume consistency between the lens and
      meta information. *)

  val field : field_name:string -> meta
  val tuple : nth:int -> arity:int -> meta
  val constructor_regular : name:string -> nth:int -> arity:int -> meta
  val constructor_inline : name:string -> field_name:string -> meta
  val list : nth:int -> meta
  val array : nth:int -> meta
end
[@@ocaml.deprecated "do not use this module directly"]
