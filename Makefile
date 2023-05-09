.PHONY: test play check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

# TODO: currently not working, will resolve later
# utop:
# 	OCAMLRUNPARAM=b dune utop lib

# Jun: you can add an image target here if you would like

play:
	OCAMLRUNPARAM=b dune exec play/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

clean:
	dune clean
	rm -f maze_generator.zip

