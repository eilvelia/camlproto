.PHONY: default
default: build

.PHONY: codegen
codegen:
	npm run codegen --prefix codegen

.PHONY: build
build:
	dune build @install @examples

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: distrib
distrib:
	dune-release distrib

.PHONY: lock
lock:
	opam lock
