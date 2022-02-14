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
  let after =
    Location.{ txt = (); loc = { loc with loc_end = loc.loc_start }}
  in
  let before =
    Location.{ txt = (); loc = { loc with loc_start = loc.loc_end }}
  in
  let fst = pp_token ~after ~before left in
  let snd = pp_token ~after ~before right in
  group (fst ^^ snd)

let rec pp lid =
  group (aux lid)

and aux = function
  | Lident { txt = "()"; loc } ->
    pp_empty ~loc LPAREN RPAREN
  | Lident { txt = "[]"; loc } ->
    pp_empty ~loc LBRACKET RBRACKET
  | Lident s -> pp_ident s
  | Ldot (lid, s) -> concat (pp lid) ~sep:PPrint.(dot ^^ break 0) (pp_ident s)
  | Lapply (l1, l2) -> concat (pp l1) ~sep:(break 0) (parens (pp l2))

let pp lid = hang 2 (pp lid)
