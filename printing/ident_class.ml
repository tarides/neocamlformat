open Source_parsing
open Asttypes

type t =
  | Prefix_op of string loc
  | Infix_op of string loc
  | Normal

(* Refer to:
    http://caml.inria.fr/pub/docs/manual-ocaml/lex.html#sss:lex-ops-symbols *)
let classify s =
  match s.txt with
  | "" -> assert false
  | ":="
  | "or"
  | "&"
  | "&&"
  | "!="
  | "mod"
  | "land"
  | "lor"
  | "lxor"
  | "lsl"
  | "lsr"
  | "asr"
  | "::"
    ->
    Infix_op s
  | _ ->
    match String.get s.txt 0 with
    | '!' | '?' | '~' -> Prefix_op s
    | '$'
    | '&'
    | '*'
    | '+'
    | '-'
    | '/'
    | '='
    | '>'
    | '@'
    | '^'
    | '|'
    | '%'
    | '<'
    | '#'
      ->
      Infix_op s
    | _ -> Normal
