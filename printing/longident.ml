open Document
open Source_parsing

include Longident

let pp_ident s =
  str s
(* Following is now handled from the parser: *)

(*
  match Ident_class.classify s with
  | Normal -> str s
  | Infix_op { loc; txt } when txt <> "" && String.get txt 0 = '*' ->
    parens (string ~loc (" " ^ txt ^ " "))
  | Infix_op _ | Prefix_op _ -> str s
       *)
let pp_empty ~(loc:Location.t) left right =
  let fst = Token.pp ~inside:loc left in
  let snd = Token.pp ~inside:loc right in
  group (fst ^^ snd)

let rec pp lid =
  group (aux lid)

and aux = function
  | Lident { txt = "()"; loc } -> pp_empty ~loc LPAREN RPAREN
  | Lident { txt = "[]"; loc } -> pp_empty ~loc LBRACKET RBRACKET
  | Lident s -> pp_ident s
  | Ldot (lid, s) ->
    let prefix = pp lid in
    let tail = pp_ident s in
    let dot = Token.pp ~after:prefix ~before:tail DOT in
    prefix ^^ dot ^^ break 0 ^^ tail
  | Lapply (l1, l2, rparen_loc) ->
    let d1 = pp l1 in
    let d2 = pp l2 in
    let lparen = Token.pp ~after:d1 ~before:d2 LPAREN in
    let rparen = Token.pp ~inside:rparen_loc RPAREN in
    d1 ^^ break 0 ^^ lparen ^^ d2 ^^ rparen

let pp lid =
  hang 2 (pp lid)
