build:
	dune build

test: build
	dune exec -- neocamlformat ./parsing/source_tree.mli ./printing/print_source.ml

clean:
	dune clean

.PHONY: clean build test
