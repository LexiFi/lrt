Dynt - Dynamic Types for OCaml
==============================

TODO:
* pitch
* opam release
* interface documentation

## Getting started

The best way to install this package is via opam.

```sh
opam insall dynt
```

Then, in order to generate runtime representations of your dynamic
types, you have to enable the dynt ppx. Your dune file might look like
the following.

```dune
(executable
  (name foo)
  (libraries bar dynt)
  (preprocess (pps dynt.deriving)))
```

Now you can use dynamic types in your `foo.ml`:

```ocaml
open Dynt

type nat =
  | Z
  | S of nat
  [@@deriving t]

let () =
  Print.print ~t:nat_t (S (S (S Z)))
```

Having the basic things set up, you are ready to explore the
[documentation][docs] of the `Dynt` module.

## About

This approach to runtime types was initially developed by [LexiFi][lexifi]
for its internal use. In 2018 it was partially reworked and prepared for
public release as [part of an internship][story].

The package is is licensed by LexiFi under the terms of the MIT license.

[docs]: https://lexifi.github.io/dynt/dynt/Dynt/index.html
[lexifi]: https://lexifi.github.io/
[story]: https://lexifi.github.io/404

## Notes to the future maintainer

There are four main directories:

* `lib` contains all the dynamic types and useful modules built atop.
* `ppx` contains the syntax extension.
* `bench` contains the json/variant roundtrip benchmark code.
* `tests` contains the separate tests. Some more tests are placed
  directly in the corresponding modules.

Everything was build and tested on `4.07.1` only.

Some things remain open:

* Except of `Json` no module uses the latest features of `Xtype` and
  `Matcher`.
* `Unify` was written before `Matcher` and needs an update.
* `git grep TODO`

Useful commands:

* View the generated code using
  `dune exec ppx/standalone.exe test/ppx.ml`.
* Run tests using `dune runtest`.
* Format the code using `dune build @fmt --auto-promote` or
  `make fmt`.
* Generate documentation for github pages using `make docs`, then commit
  and push to master.
