build:
	dune build
exec:
	dune exec bin/main.exe
install:
	opam install . --deps-only --working-dir

.PHONY: build exec install
