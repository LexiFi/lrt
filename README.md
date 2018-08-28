# dynamic_types

## [@@deriving t]

`test/ppx.ml` contains inline tests for the PPX deriver. The tests can
be evaluated using `dune runtest`.

The generated code can be viewed using one of the following commands.

```
dune exec ppx/standalone.exe test/ppx.ml
{ echo '(* vim: set ft=ocaml : *)'; dune exec ppx/standalone.exe test/ppx.ml } | vim -R -
```
