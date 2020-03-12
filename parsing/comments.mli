type t

val init : unit -> t

val between : Lexing.position -> Lexing.position -> unit -> string list
