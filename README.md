A reimplementation of [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) to
explore different design choices:
- use of a surface AST representing the concrete syntax, instead of OCaml's
  `Parsetree`
- use of `PPrint` instead of `Format`

It currently lacks all the nice infrastructure of OCamlformat:
- a testsuite
- a check that the result parses to the same Parsetree as the input
- integration with odoc, and proper formatting of comments

# Implementation details

## The surface AST, and the parser

Started by taking a snapshot of OCaml's 4.10 parser and parsetree. And then
reintroduce AST nodes for concrete syntaxes that were desugared by the parser.

In the parser itself, my initial plan was to not change any rule, only the
semantic actions. Eventually I had to give up on that, for somewhat
disappointing reason: I'm storing more locations in the AST than the ocaml
parser & parsetree do. I could have done so without modifying any rules, but the
`mkrhs` were either getting in the way, or much more convenient to add...

It might still be worth it to revert back to the original plan (if possible).

## Coding style for the formatter

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

For an illustration of what I mean by this, look at the `Application` module in
[`printing/print_source.ml`](./printing/print_source.ml).

## Comments placement

#### The compiler, OCamlformat, Reason

Currently, OCaml's parser explicitely fetches docstrings and attaches them to
nodes when building the AST (i.e. in the semantic actions, not in the rules).
OCamlformat explicitely fetches comments and includes them in the formatting
queue while formatting the AST.

This sort of approach seems tedious, error-prone (it's easy to forget / misplace
comments) and makes the code harder to read (IMHO).

I have heard that reason inserts comment in its AST, so there should be no
special logic for them in the printer?
But I haven't seen the code which does that; I expect it works similarly to the
approach described above (and has the same drawbacks).

Furthermore, this last approach seems to restrict positions in which comments
can appear, which I find undesirable: comments are not just a way to attach
documentation to code, they are also used to temporarily disable bits of code.
For that particular use, it can be annoying if your disabled bit of code gets
moved away: you'll have to manually move it back when uncommenting it.

#### Approach taken here

For the record: modulo the very minor annoyance just mentioned (the moving of
commented out code), both approaches seems to be giving good results!  
Nevertheless, I find that they make the (formatting) code harder to work with,
so I decided to try a different way of doing things.

The way comments are inserted in the document in this reimplementation is
completely implicit: you will not find anything mentioning comments in [the AST
formatter](./printing/print_source.ml).

This is possible because PPrint works by concatenating documents, instead of
pushing printing commands into a queue. If that doesn't mean much to you, I
encourage you to read
[this small introduction](http://parce-q.eu/~trefis/pprint-doc/pprint/index.html#taste).

So if you decide to restrict atomic documents to source tokens, then
concatenation becomes the perfect time to insert comments! And the formatting
code doesn't even have to care about comments at all.

I added [a small wrapper around PPrint](./printing/document.ml), so that
documents are now annotated with a location. And the concatenation operation now
looks at the location of both documents, fetches all the comments that appear
between these two location, and inserts them in the middle.

If you have a location for all the tokens (which is relatively easy to get as
OCamlformat already proved), then you can place all the comments in the exact
same relative position as in the source (i.e. between the same two tokens).

# POST-MORTEM UPDATE

- as a follow up to the previous paragraphs, I initially I wrote this:
  > *Caveat:* this works well for "separating" tokens, i.e. keywords (`let`, `with`,
  > `in`, ...) and punctuation (`.`, `;`, `::`, ...) but the model breaks down for
  > parentheses. That's because we will sometimes synthesize parentheses tokens
  > which weren't present in the source.
  > Currently this means that edge comments will always appear outside of
  > synthesized parentheses, which is a bit unfortunate. Ideas welcome!

  but eventually I came to realize that the formatter should just change the
  layout/whitespaces of its input, but nothing else. There should be a separate
  "linter"/"normalizer" if we want to synthetize tokens and such. This is briefly
  touched upon in
  [docs/ocaml-workshop-2022_rejected.pdf](docs/ocaml-workshop-2022_rejected.pdf).
- the HEAD of this repo relies on a custom version of `PPrint`, so don't expect
  it the last few commits to build.
