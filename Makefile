.PHONY: default
default: build

.PHONY: build
build:
	dune build @install @examples

.PHONY: clean-build
clean-build: clean build

.PHONY: test
test:
	dune runtest

# COVERAGE_FILES := \
# 	--expect src/tl/lib/ \
# 	--expect src/math/

.PHONY: coverage
coverage:
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html
	bisect-ppx-report summary
	@echo See _coverage/index.html

.PHONY: clean
clean:
	dune clean
	rm -rf _coverage/

.PHONY: distrib
distrib:
	dune-release distrib

.PHONY: lock
lock:
	opam lock ./camlproto.opam
