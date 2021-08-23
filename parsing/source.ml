type t = string

let source = ref ""

let print_tok : Parser.token -> string = function
  | AMPERAMPER -> "&&"
  | AMPERSAND -> "&"
  | AND -> "and"
  | AS -> "as"
  | ASSERT -> "assert"
  | BACKQUOTE -> "`"
  | BANG -> "!"
  | BAR -> "|"
  | BARBAR -> "||"
  | BARRBRACKET -> "|]"
  | BEGIN -> "begin"
  | CHAR _ -> "CHAR"
  | CLASS -> "class"
  | COLON -> ":"
  | COLONCOLON -> "::"
  | COLONEQUAL -> ":="
  | COLONGREATER -> ":>"
  | COMMA -> ","
  | CONSTRAINT -> "constraint"
  | DO -> "do"
  | DONE -> "done"
  | DOT -> "."
  | DOTDOT -> ".."
  | DOWNTO -> "downto"
  | ELSE -> "else"
  | END -> "end"
  | EOF -> "EOF"
  | EQUAL -> "="
  | EXCEPTION -> "exception"
  | EXTERNAL -> "external"
  | FALSE -> "false"
  | FLOAT _ -> "FLOAT"
  | FOR -> "for"
  | FUN -> "fun"
  | FUNCTION -> "function"
  | FUNCTOR -> "functor"
  | GREATER -> ">"
  | GREATERRBRACE -> ">}"
  | GREATERRBRACKET -> ">]"
  | IF -> "if"
  | IN -> "in"
  | INCLUDE -> "include"
  | INFIXOP0 _ -> "INFIXOP0"
  | INFIXOP1 _ -> "INFIXOP1"
  | INFIXOP2 _ -> "INFIXOP2"
  | INFIXOP3 _ -> "INFIXOP3"
  | INFIXOP4 _ -> "INFIXOP4"
  | DOTOP op -> "." ^ op
  | LETOP _ -> "LETOP"
  | ANDOP _ -> "ANDOP"
  | INHERIT -> "inherit"
  | INITIALIZER -> "initializer"
  | INT _ -> "INT"
  | LABEL _ -> "LABEL"
  | LAZY -> "lazy"
  | LBRACE -> "{"
  | LBRACELESS -> "{<"
  | LBRACKET -> "["
  | LBRACKETBAR -> "[|"
  | LBRACKETLESS -> "[<"
  | LBRACKETGREATER -> "[>"
  | LBRACKETPERCENT -> "[%"
  | LBRACKETPERCENTPERCENT -> "[%%"
  | LESS -> "<"
  | LESSMINUS -> "<-"
  | LET -> "let"
  | LIDENT _ -> "LIDENT"
  | LPAREN -> "("
  | LBRACKETAT -> "[@"
  | LBRACKETATAT -> "[@@"
  | LBRACKETATATAT -> "[@@@"
  | MATCH -> "match"
  | METHOD -> "method"
  | MINUS -> "-"
  | MINUSDOT -> "-."
  | MINUSGREATER -> "->"
  | MODULE -> "module"
  | MUTABLE -> "mutable"
  | NEW -> "new"
  | NONREC -> "nonrec"
  | OBJECT -> "object"
  | OF -> "of"
  | OPEN -> "open"
  | OPTLABEL _ -> "OPTLABEL"
  | OR -> "or"
  | PERCENT -> "%"
  | PLUS -> "+"
  | PLUSDOT -> "+."
  | PLUSEQ -> "+="
  | PREFIXOP _ -> "PREFIXOP"
  | PRIVATE -> "private"
  | QUESTION -> "?"
  | QUOTE -> "'"
  | RBRACE -> "}"
  | RBRACKET -> "]"
  | REC -> "rec"
  | RPAREN -> ")"
  | SEMI -> ";"
  | SEMISEMI -> ";;"
  | HASH -> "#"
  | HASHOP _ -> "HASHOP"
  | SIG -> "sig"
  | STAR -> "*"
  | STRING _ -> "STRING"
  | STRUCT -> "struct"
  | THEN -> "then"
  | TILDE -> "~"
  | TO -> "to"
  | TRUE -> "true"
  | TRY -> "try"
  | TYPE -> "type"
  | UIDENT _ -> "UIDENT"
  | UNDERSCORE -> "_"
  | VAL -> "val"
  | VIRTUAL -> "virtual"
  | WHEN -> "when"
  | WHILE -> "while"
  | WITH -> "with"
  | COMMENT _ -> "COMMENT"
  | DOCSTRING _ -> "DOCSTRING"
  | EOL -> "EOL"

let source_between (start : Lexing.position) (stop : Lexing.position) tok =
  let pos = start.pos_cnum in
  let len = stop.pos_cnum - start.pos_cnum in
  begin try
      assert (pos >= 0);
      assert (len > 0);
    with exn ->
      Format.eprintf "source_between %d:%d -- %d:%d: %s@."
        start.pos_lnum (start.pos_cnum - start.pos_bol)
        stop.pos_lnum (stop.pos_cnum - stop.pos_bol)
        (print_tok tok) ;
      raise exn
  end;
  String.sub !source pos len

let lexbuf_set_pos lexbuf pos =
  lexbuf.Lexing.lex_abs_pos <- pos.Lexing.pos_cnum ;
  lexbuf.lex_curr_p <- pos

let loc_of_token_between ~start ~stop token =
  let sub = source_between start stop token in
  let lexbuf = Lexing.from_string sub in
  Lexer.init ();
  lexbuf_set_pos lexbuf start;
  let rec loop () =
    match Lexer.token lexbuf with
    | Parser.EOF -> raise Not_found
    | tok when tok = token -> Location.curr lexbuf
    | _ -> loop ()
  in
  loop ()
