LexiFi runtime types
====================

[//]: # (Remember to keep this pitch in sync with lib/lrt.ml and lrt.opam)

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

## Getting started

This package has not been made available on the OPAM repository. You can 
still use OPAM to install its current version: 

```sh
opam pin add ocaml-vdom git://github.com/lexifi/lrt.git
```

Then, in order to generate runtime representations of your OCaml
types, you have to enable the lrt ppx. Your dune file might look like
the following.

```dune
(executable
  (name foo)
  (libraries bar lrt)
  (preprocess (pps lrt.deriving)))
```

Now you can use runtime types in your programs:

```ocaml
open Lrt

type nat =
  | Z
  | S of nat
  [@@deriving t]

let () =
  Print.show ~t:nat_t (S (S (S Z)))
```

Having the basic things set up, you are ready to explore the
[documentation][docs] of the `Lrt` module.

## About

This must be considered a **preliminary, potentially unstable release**.

The package is is licensed by LexiFi under the terms of the MIT license.

[docs]: https://lexifi.github.io/lrt/lrt/Lrt/index.html
[lexifi]: https://lexifi.github.io/

## Maintaining

There are four main directories:

* `lib` contains the runtime type representations and useful modules built atop.
* `ppx` contains the syntax extension.
* `bench` contains the json/variant roundtrip benchmark code.
* `tests` contains the separate tests. Some more tests are placed
  directly in the corresponding modules.

Some things remain open:

* Except of `Json` no module uses the latest features of `Xtype` and
  `Matcher`.
* `Unify` was written before `Matcher` and uses a different definition
  of equality.
* `git grep TODO`

Useful commands:

* View the generated code using
  `dune exec ppx/standalone.exe test/ppx.ml`.
* Run tests using `dune runtest`.
* Format the code using `dune build @fmt --auto-promote` or
  `make fmt`.
* Generate documentation for github pages using `make docs`, then commit
  and push to master.
