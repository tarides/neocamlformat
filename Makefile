pp: main.ml
	dune build
	@-ln -s ./_build/default/main.exe $@

test: pp
	./pp ~/repos/ocaml/trunk/parsing/parsetree.mli

clean:
	dune clean
	@- rm pp

.PHONY: clean pp test
