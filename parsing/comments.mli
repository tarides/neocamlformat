type t

val init : (string * Location.t) list  -> unit

val compare_pos: Lexing.position -> Lexing.position -> int

val between : Lexing.position -> Lexing.position -> unit ->
  (string * Location.t) list

val before : Lexing.position -> (string * Location.t) list
val after : Lexing.position -> (string * Location.t) list

val report_remaining : unit -> unit
