type t = string

let source = ref ""

let print_tok : Parser.token -> string = function
  | AMPERAMPER -> "AMPERAMPER"
  | AMPERSAND -> "AMPERSAND"
  | AND -> "AND"
  | AS -> "AS"
  | ASSERT -> "ASSERT"
  | BACKQUOTE -> "BACKQUOTE"
  | BANG -> "BANG"
  | BAR -> "BAR"
  | BARBAR -> "BARBAR"
  | BARRBRACKET -> "BARRBRACKET"
  | BEGIN -> "BEGIN"
  | CHAR _ -> "CHAR"
  | CLASS -> "CLASS"
  | COLON -> "COLON"
  | COLONCOLON -> "COLONCOLON"
  | COLONEQUAL -> "COLONEQUAL"
  | COLONGREATER -> "COLONGREATER"
  | COMMA -> "COMMA"
  | CONSTRAINT -> "CONSTRAINT"
  | DO -> "DO"
  | DONE -> "DONE"
  | DOT -> "DOT"
  | DOTDOT -> "DOTDOT"
  | DOWNTO -> "DOWNTO"
  | ELSE -> "ELSE"
  | END -> "END"
  | EOF -> "EOF"
  | EQUAL -> "EQUAL"
  | EXCEPTION -> "EXCEPTION"
  | EXTERNAL -> "EXTERNAL"
  | FALSE -> "FALSE"
  | FLOAT _ -> "FLOAT"
  | FOR -> "FOR"
  | FUN -> "FUN"
  | FUNCTION -> "FUNCTION"
  | FUNCTOR -> "FUNCTOR"
  | GREATER -> "GREATER"
  | GREATERRBRACE -> "GREATERRBRACE"
  | GREATERRBRACKET -> "GREATERRBRACKET"
  | IF -> "IF"
  | IN -> "IN"
  | INCLUDE -> "INCLUDE"
  | INFIXOP0 _ -> "INFIXOP0"
  | INFIXOP1 _ -> "INFIXOP1"
  | INFIXOP2 _ -> "INFIXOP2"
  | INFIXOP3 _ -> "INFIXOP3"
  | INFIXOP4 _ -> "INFIXOP4"
  | DOTOP _ -> "DOTOP"
  | LETOP _ -> "LETOP"
  | ANDOP _ -> "ANDOP"
  | INHERIT -> "INHERIT"
  | INITIALIZER -> "INITIALIZER"
  | INT _ -> "INT"
  | LABEL _ -> "LABEL"
  | LAZY -> "LAZY"
  | LBRACE -> "LBRACE"
  | LBRACELESS -> "LBRACELESS"
  | LBRACKET -> "LBRACKET"
  | LBRACKETBAR -> "LBRACKETBAR"
  | LBRACKETLESS -> "LBRACKETLESS"
  | LBRACKETGREATER -> "LBRACKETGREATER"
  | LBRACKETPERCENT -> "LBRACKETPERCENT"
  | LBRACKETPERCENTPERCENT -> "LBRACKETPERCENTPERCENT"
  | LESS -> "LESS"
  | LESSMINUS -> "LESSMINUS"
  | LET -> "LET"
  | LIDENT _ -> "LIDENT"
  | LPAREN -> "LPAREN"
  | LBRACKETAT -> "LBRACKETAT"
  | LBRACKETATAT -> "LBRACKETATAT"
  | LBRACKETATATAT -> "LBRACKETATATAT"
  | MATCH -> "MATCH"
  | METHOD -> "METHOD"
  | MINUS -> "MINUS"
  | MINUSDOT -> "MINUSDOT"
  | MINUSGREATER -> "MINUSGREATER"
  | MODULE -> "MODULE"
  | MUTABLE -> "MUTABLE"
  | NEW -> "NEW"
  | NONREC -> "NONREC"
  | OBJECT -> "OBJECT"
  | OF -> "OF"
  | OPEN -> "OPEN"
  | OPTLABEL _ -> "OPTLABEL"
  | OR -> "OR"
  | PERCENT -> "PERCENT"
  | PLUS -> "PLUS"
  | PLUSDOT -> "PLUSDOT"
  | PLUSEQ -> "PLUSEQ"
  | PREFIXOP _ -> "PREFIXOP"
  | PRIVATE -> "PRIVATE"
  | QUESTION -> "QUESTION"
  | QUOTE -> "QUOTE"
  | RBRACE -> "RBRACE"
  | RBRACKET -> "RBRACKET"
  | REC -> "REC"
  | RPAREN -> "RPAREN"
  | SEMI -> "SEMI"
  | SEMISEMI -> "SEMISEMI"
  | HASH -> "HASH"
  | HASHOP _ -> "HASHOP"
  | SIG -> "SIG"
  | STAR -> "STAR"
  | STRING _ -> "STRING"
  | STRUCT -> "STRUCT"
  | THEN -> "THEN"
  | TILDE -> "TILDE"
  | TO -> "TO"
  | TRUE -> "TRUE"
  | TRY -> "TRY"
  | TYPE -> "TYPE"
  | UIDENT _ -> "UIDENT"
  | UNDERSCORE -> "UNDERSCORE"
  | VAL -> "VAL"
  | VIRTUAL -> "VIRTUAL"
  | WHEN -> "WHEN"
  | WHILE -> "WHILE"
  | WITH -> "WITH"
  | COMMENT _ -> "COMMENT"
  | DOCSTRING _ -> "DOCSTRING"
  | EOL -> "EOL"

let source_between (start : Lexing.position) (stop : Lexing.position) tok =
  let pos = start.pos_cnum in
  let len = stop.pos_cnum - start.pos_cnum in
  begin try
      assert (pos > 0);
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
