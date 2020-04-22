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
  | Rangle
  | Lbracket
  | Open_variant
  | Closed_variant
  | Rbracket
  | If
  | Inherit
  | Bang
  | Virtual
  | Val
  | Mutable
  | Method
  | Private
  | Constraint
  | Initializer
  | Class
  | And
  | Type

let to_parser_token t : Source_parsing.Parser.token =
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
  | Rangle -> GREATER
  | Lbracket -> LBRACKET
  | Open_variant -> LBRACKETGREATER
  | Closed_variant -> LBRACKETLESS
  | Rbracket -> RBRACKET
  | If -> IF
  | Inherit -> INHERIT
  | Bang -> BANG
  | Virtual -> VIRTUAL
  | Val -> VAL
  | Mutable -> MUTABLE
  | Method -> METHOD
  | Private -> PRIVATE
  | Constraint -> CONSTRAINT
  | Initializer -> INITIALIZER
  | Class -> CLASS
  | And -> AND
  | Type -> TYPE

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
  | Rangle -> ">"
  | Lbracket -> "["
  | Rbracket -> "]"
  | Open_variant -> "[>"
  | Closed_variant -> "[<"
  | If -> "if"
  | Inherit -> "inherit"
  | Bang -> "!"
  | Virtual -> "virtual"
  | Val -> "val"
  | Mutable -> "mutable"
  | Method -> "method"
  | Private -> "private"
  | Constraint -> "constraint"
  | Initializer -> "initializer"
  | Class -> "class"
  | And -> "and"
  | Type -> "type"
