type t =
  | Colon
  | Qmark
  | Equals
  | Coerce
  | In
  | When
  | Rarrow
  | Larrow
  | Do
  | Dot
  | Dotdot
  | Of
  | As
  | Cons
  | Pipe
  | With
  | Then
  | Else
  | Semi
  | To
  | Downto
  | Sharp
  | Colonequals
  | Plusequals
  | Star
  | While
  | Done

let to_parser_token t : Source_parsing.Source_parser.token =
  match t with
  | Colon -> COLON
  | Qmark -> QUESTION
  | Equals -> EQUAL
  | Coerce -> COLONGREATER
  | In -> IN
  | When -> WHEN
  | Rarrow -> MINUSGREATER
  | Larrow -> LESSMINUS
  | Do -> DO
  | Dot -> DOT
  | Dotdot -> DOTDOT
  | Of -> OF
  | As -> AS
  | Cons -> COLONCOLON
  | Pipe -> BAR
  | With -> WITH
  | Then -> THEN
  | Else -> ELSE
  | Semi -> SEMI
  | To -> TO
  | Downto -> DOWNTO
  | Sharp -> HASH
  | Colonequals -> COLONEQUAL
  | Plusequals -> PLUSEQ
  | Star -> STAR
  | While -> WHILE
  | Done -> DONE

let to_string = function
  | Colon -> ":"
  | Qmark -> "?"
  | Equals -> "="
  | Coerce -> ":>"
  | In -> "in"
  | When -> "when"
  | Rarrow -> "->"
  | Larrow -> "<-"
  | Do -> "do"
  | Dot -> "."
  | Dotdot -> ".."
  | Of -> "of"
  | As -> "as"
  | Cons -> "::"
  | Pipe -> "|"
  | With -> "with"
  | Then -> "then"
  | Else -> "else"
  | Semi -> ";"
  | To -> "to"
  | Downto -> "downto"
  | Sharp -> "#"
  | Colonequals -> ":="
  | Plusequals -> "+="
  | Star -> "*"
  | While -> "while"
  | Done -> "done"
