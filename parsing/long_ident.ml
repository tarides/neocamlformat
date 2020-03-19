open Asttypes

type t = 
  | Lident of string loc
  | Ldot of t * string loc
  | Lapply of t * t

let last = function
    Lident s -> s
  | Ldot(_, s) -> s
  | Lapply(_, _) -> invalid_arg "Longident.last"
