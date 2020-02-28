pp: main.ml
	dune build
	ln -s ./_build/default/main.exe $@

clean:
	dune clean
	@- rm pp
.PHONY: clean
