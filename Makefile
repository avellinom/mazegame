.PHONY: test play check image

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

# TODO: currently not working, will resolve later
# utop:
# 	OCAMLRUNPARAM=b dune utop lib

image:
	OCAMLRUNPARAM=b dune exec image/main.exe

play:
	OCAMLRUNPARAM=b dune exec play/main.exe

tree:
	dune exec image/main.exe

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

clean:
	dune clean
	rm -f maze_generator.zip

zip:
	rm -f game.zip
	zip -r game.zip . -x@exclude.lst

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

