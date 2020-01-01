.PHONY: default
default: build

.PHONY: build
build:
	dune build @install @examples

.PHONY: test
test:
	dune runtest

COVERAGE_FILES := \
	--expect src/tl2/tl-lib/

.PHONY: coverage
coverage: clean
	BISECT_ENABLE=yes dune build
	dune runtest --no-buffer
	bisect-ppx-report html $(COVERAGE_FILES)
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
