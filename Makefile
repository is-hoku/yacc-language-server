build:
	dune build
exec: build
	dune exec bin/main.exe
install:
	opam install . --deps-only --working-dir
test:
	dune runtest

.PHONY: build exec install test
