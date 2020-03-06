A reimplementation of [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) to
explore different design choices:
- use of a surface AST representing the concrete syntax, instead of OCaml's
  `Parsetree`
- use of `PPrint` instead of `Format`

# Implementation details

## The surface AST, and the parser

Started by taking a snapshot of OCaml's 4.10 parser and parsetree. And then
reintroduce AST nodes for concrete syntaxes that were desugared by the parser.

In the parser itself, I haven't changed any rule, only the semantic actions.  
In the long run, this might be a bad idea.

## The printing code

### Coding style

There's a tension between:
- very ad-hoc formatting code (ocamlformat's style), which gives a "pretty"
  output
- very generic formatting code, which gives an ugly output

I have tried to adopt a coding style similar to what one would do when using
[factor](https://factorcode.org/) or [Forth](https://www.forth.com/forth/):
almost never write big chunks of code, instead define small specialized
combinators, and … combine them.

This of course doesn't always work, because some formatting decisions will
depend on a collection of elements. But that's the spirit.

### Comments placement

**TODO!**

The ideas floating around:
- adding them in the AST (reason does that I believe)
- fetching them explicitely, using locations during printing
- attach locations to PPrint's documents, and fetching them implicitely when
  catenating docs.
