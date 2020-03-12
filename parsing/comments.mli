type t

val init : unit -> t

val between : Lexing.position -> Lexing.position -> unit -> string list

val report_remaining : unit -> unit
