build:
	dune build @install

test: build
	dune test

clean:
	dune clean

.PHONY: clean build test
