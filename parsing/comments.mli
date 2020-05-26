type t

val init : unit -> t

val between : Lexing.position -> Lexing.position -> unit ->
  (string * Location.t) list

val before : Lexing.position -> (string * Location.t) list
val after : Lexing.position -> (string * Location.t) list

val report_remaining : unit -> unit
