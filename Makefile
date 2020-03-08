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
# 	--expect src/tl/tl-lib/ \
# 	--expect src/math/

.PHONY: coverage
coverage: clean
	BISECT_ENABLE=yes dune runtest --force
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
	opam lock
