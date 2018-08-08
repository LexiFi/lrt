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

deploy-docs: 
	test -d _site || git worktree add _site gh-pages
	cd _site && git rm -rf *
	cp -r _build/default/_doc/_html/* _site
	cd _site && git add --all
	cd _site && git commit -m "Deploy updates"
	git push origin gh-pages
	git worktree remove _site

