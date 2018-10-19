.PHONY: build test fmt docs

build:
	dune build

test:
	dune runtest

fmt: test
	dune build @fmt --auto-promote

docs:
	dune build @doc
	rm -r docs || true
	cp -r _build/default/_doc/_html docs
