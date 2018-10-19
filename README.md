Dynt - dynamic types for OCaml
==============================

TODO:
* pitch
* install notes
* github pages with documentation
* opam package
* interface documentation
* license, file headers

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
  `make format`.
* Generate documentation for github pages using `make docs`, then commit
  and push to master.
