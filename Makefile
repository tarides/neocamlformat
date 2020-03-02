pp: main.ml
	dune build
	@-ln -s ./_build/default/main.exe $@

test: pp
	./pp ./source_tree.mli
	./pp ./print_source.ml

clean:
	dune clean
	@- rm pp

.PHONY: clean pp test
