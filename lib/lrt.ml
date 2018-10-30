(******************************************************************************)
(*  Copyright (C) 2018 by LexiFi.                                             *)
(*                                                                            *)
(*  This source file is released under the terms of the MIT license as part   *)
(*  of the lrt package. Details can be found in the attached LICENSE file.    *)
(******************************************************************************)

(** LexiFi runtime types. *)

(*  REMEMBER TO KEEP THE FOLLOWING PITCH IN SYNC WITH THE README AND OPAM FILE. *)

(** {3 Introduction}

    It is often useful to get access to types at runtime in order to implement
    generic type-driven operations. A typical example is a generic
    pretty-printer. Unfortunately, the OCaml compiler does not keep type
    information at runtime. At LexiFi, we have extended OCaml to support runtime
    types. This extension has been in use for years and is now a key element in
    many of our interesting components, such as our automatic GUI framework
    (which derives GUIs from type definitions) or our high-level database layer
    (which derives SQL schema from type definitions, and exposes a well-typed
    interface for queries). This extension is tightly integrated with the OCaml
    typechecker, which allows the compiler to synthesize the runtime type
    representations with minimal input from the programmer.

    This package makes the features of our extension available to other OCaml
    users without relying on a modified compiler. Instead, it only relies on a
    PPX syntax extension that synthesizes the runtime representation of types
    from their syntactic definition with a deriving-like approach.

    Based on this new implementation we are able to open-source the
    infrastructure we have developed around the machinery of runtime types as
    well as libraries built upon them.
*)

(** {3 Build runtime types}

    Runtime representations of OCaml types are built using the [lrt.deriving]
    PPX syntax extension. In the simplest case, you only have to attach a
    [ [@@deriving t] ] attribute to the type declaration.

    {[
    # #require "lrt.deriving";;
    # open Lrt.Std;;
    # type foo = { bar: string } [@@deriving t] ;;
    type foo = { bar: string }
    val foo_t : foo ttype
    # type t = foo * int [@@deriving t] ;;
    val t : t ttype
    ]}

    Runtime representations of the basic OCaml types can be found in the {!Std}
    module. These definitions are generally required, when you use the
    [[@@deriving t]] syntax extension.
*)

module Std = struct
  include Std

  (**/**)

  module Lrt_ppx_runtime = Lrt_ppx_runtime

  (**/**)
end

(** {4 Free variable handling}

    Types with free variables are represented as closures with one
    {!ttype} argument per free variable. Most APIs, like {!Print} for dynamic
    printing, consume closed types.

    {[
    # type 'a tree =
    +   | Leave of 'a
    +   | Node of 'a tree list
    + [@@deriving t];;
    val tree_t : 'a ttype -> 'a tree ttype
    # let () = Print.show ~t:(tree_t int_t) (Node [Leave 0; Leave 1]);;
    Node [Leave 0; Leave 1]
    ]}

    Stating the types in function application style might be a bit unintuitive.
    Thus there is an extension point that translates types to applications.
    Instead of the previous example, you can also write the following.

    {[
    # let () = Print.show ~t:[%t: int tree] (Node [Leave 0; Leave 1]);;
    Node [Leave 0; Leave 1]
    ]}
*)

(** {4 Abstract types}

    We attempt to support abstract types. Whenever you want to hide the actual
    type definition from the derived ttype, you have to annotate the type
    declarations with [ [@@abstract] ].

    {[
    # module M : sig
    +   type t [@@deriving t]
    + end = struct
    +   type t = int array [@@deriving t]
    + end;;
    module M : sig type t val t : t ttype end
    # Format.printf "%a\n" Ttype.print M.t;;
    int array
    # module N : sig
    +   type t [@@deriving t]
    + end = struct
    +   type t = int array [@@abstract] [@@deriving t]
    + end;;
    module N : sig type t val t : t ttype end
    # Format.printf "%a\n" Ttype.print M.t;;
    //toplevel//.N.t
    ]}

    It is worth to note that abstract types are represented by a string. You can
    trick the naming mechanism into producing indistinguishable abstract runtime
    types for distinct OCaml types. You can bypass the name generation by
    providing a string argument to the abstract annotation.

    {[
    # type abstract = int [@@abstract "uid"] [@@deriving t];;
    val abstract_t : abstract ttype
    # Format.printf "%a\n" Ttype.print abstract_t;;
    uid
    ]}

    In case you want to expose an abstract ttype, but use a non-abstract version
    within the module, we recommend to define two types - one non-abstract for
    internal use and one abstract for satisfying the interface - as outlined
    below.

    {[
    # module M : sig
    +   type hidden [@@deriving t]
    + end = struct
    +   type visible = string list
    +   and hidden = visible [@@abstract] [@@deriving t];;
    +   (* [visible] represents a string list here. *)
    + end;;
    ]}
*)

(** {4 Patching}

    It happens frequently, that ttypes are not available under the expected
    name. For such cases, we provide the [@patch] annotation.

    {[
    # lazy_t;;
    - : 'a ttype -> 'a lazy_t ttype = <fun>
    # type 'a lazy_pair = ('a * 'a) Lazy.t [@patch lazy_t] [@@deriving t];;
    type 'a lazy_pair = ('a * 'a) lazy_t
    val lazy_pair_t : 'a ttype -> 'a lazy_pair ttype = <fun>
    ]}

    When using an external type that has no corresponding ttype we recommend to
    introduce an abstract alias and use it as replacement.

    {[
    type external = External.t [@@abstract] [@@deriving t]
    type local = (External.t [@patch external_t]) list [@@deriving t]
    ]}
*)

(** {4 Properties}

    Our runtime types support attachments of properties. The behaviour of some
    APIs can be tweaked by providing certain properties. Properties can be added
    to core types, record fields and constructors. Keep in mind the binding
    precedence of annotations.

    {[
    type sum =
      | A of int [@prop {key1= "binds to constructor A"}]
      | B of (int [@prop {key2= "binds to type int"}])
    and record =
      { c : int [@prop {key3= "binds to field c"}]
      ; d : (int [@prop {key4= "binds"; key5="to int"}])
      }
    [@@deriving t]
    ]}
*)

(** {3 Use runtime types}

    We provide some example modules that consume runtime types. The best entry
    point for further exploring the features of Lrt is probably the
    implementation of {!Json.conv}.

    {!Print} is used as generic dynamic printer. It is able to print arbitrary
    values based on their runtime type. Values of abstract types can be printed
    by registering abstract printers.

    {!Variant} may be used to serialize values in an OCaml compatible syntax.
    Provided a runtime type, the module is able to serialize and deserialize
    arbitrary values of non-abstract type. Custom (de)variantizers for abstract
    types can be registered globally.

    {!Json} provides serialization like {!Variant} but targets JSON as
    intermediate format. Additionally, it uses the latest features provided by
    {!Matcher} to allow the registration of custom converters for any type.

    {!Check} is a Quickcheck implementation that derives value generators from
    runtime types. Additionally, it is able to generate random runtime types and
    thereby values of random type. This is useful for testing functions that are
    meant to handle any type.
*)

module Print = Print
module Variant = Variant
module Json = Json
module Check = Check

(** {4 Type representation}

    Lrt comes with different representations of runtime types. Depending on the
    application, one might use one or another.

    {!Stype.t} or {!stype} in short are an untyped runtime representation of
    OCaml types. Stypes are easy to construct, serializable and allow to write
    unsafe but powerful code. Most users want to avoid this interface.

    {!Ttype.t} or {!ttype} in short extend the untyped {!stype} with an OCaml
    type. Ttypes can be built using the [[@@deriving t]] syntax extension
    and can be used to safely consume APIs that make use of runtime types.

    {!Xtype.t} enable safe inspection of runtime types. Xtypes are used to
    implement APIs that make use of runtime types.
*)

module Stype = Stype
module Ttype = Ttype
module Xtype = Xtype

(** {4 Unification}

    The {!Unify} module holds functors that allow to unify an unclosed runtime
    type with a closed one. This was particularly interesting for implementing
    the abstract type handling of {!Print} and {!Variant}. It may be that
    {!Matcher} is strictly more powerful and {!Unify} can be dropped.
*)

module Unify = Unify

(** {4 Pattern matching}

    {!Matcher} provides a mechanism for storing data indexed by type
    using as discrimination tree. The runtime type provided as key during
    insertion may contain free variables. Data can be retrieved from the store
    by providing a closed type. During retrieval, the key type is unified with
    the type used for insertion.

    This provides a mechanism similar to OCaml pattern matching but for runtime
    types.
*)

module Matcher = Matcher

(** {4 Type equality}

    Some of the other modules are able to check for type equality of dynamically
    crafted types. Such type equalities are inherently out of reach for the
    OCaml type system. They are "transported back" with help of the {!TypEq}
    module.

    A value of type [('a, 'b) TypEq.t] can be interpreted as equality proof for
    ['a] and ['b]. OCaml's type system accepts this proof when you open the
    GADT constructor {!TypEq.Eq}. An unwrap may look like the following.

    {[
      let plus: type a. int -> a -> (a, int) TypEq.t -> int =
       fun a b eq ->
        let TypEq.Eq = eq in
        a + b
    ]}
*)

module TypEq = TypEq

(** {3 Paths}

    We include an implementation of lenses and list of lenses: {!Path} enables
    access to values in nested tuples, records and constructors.  Additionally,
    paths can be used to access nested types (see {!Xtype.project_path}).

    Paths can be built using the [lrt.path] syntax extension.

    {[
      # #require "lrt.path";;
      # type t = A of {b: int array list * string}
      + let p1 : (t, string) Path.t = [%path? [ A b; (_, []) ]]
      + let p2 : (t, int)    Path.t = [%path? [ A b; ([], _); [0]; [|1|] ]]
      + let Path.{get; set} = Path.lens p2
      + let () =
          if get (A {b= ([ [|0; 42|]; [||] ], "clutter")}) = Some 42
          then print_endline "success" ;;
      success
    ]}

    Further instructions can be found within the {!Path} module.
*)

module Path = Path

(** {3 open Lrt}

    We recommend to place [open Lrt] at the toplevel of your modules to have
    the runtime representation of basic OCaml types and all the lrt tools
    available when you need them. If you do not want to have the [Lrt.*]
    modules cluttering your namespace use [open Lrt.Std].
*)

type stype = Stype.t
type 'a ttype = 'a Ttype.t
type dynamic = Ttype.dynamic = Dyn : 'a ttype * 'a -> dynamic

include Std
