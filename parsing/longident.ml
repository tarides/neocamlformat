open Asttypes

type t = 
  | Lident of string loc
  | Ldot of t * string loc
  | Lapply of t * t * Location.t

let last = function
    Lident s -> s
  | Ldot(_, s) -> s
  | Lapply(_, _, _) -> invalid_arg "Longident.last"

let endpos = function
  | Lident s
  | Ldot(_, s) -> s.loc
  | Lapply(_, _, loc) -> loc
