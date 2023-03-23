.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop lib

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	dune build
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f maze_generator.zip

