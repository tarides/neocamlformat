open Source_parsing
open Location

let comment (s, (loc : Location.t)) =
  let loc_start =
    { loc.loc_start with  pos_cnum = loc.loc_start.pos_cnum + 2 }
  in
  let open PPrint in
  match s with
  | "*" when loc.loc_end.pos_cnum - loc.loc_start.pos_cnum = 4 ->
    { txt = !^"(**)"; loc }
  | _ ->
    let l = { loc with  loc_start } in
    let txt =
      !^"(*" ^^ Verbatim.pp_string ~comment:true ~adjust_indent:l s ^^ !^"*)"
    in
    { txt; loc }

type lfr = Left | Float | Right

module C = Source_parsing.Comments

let decide_placement p1 p2 (l : Location.t) =
  Printf.eprintf "CMT %d,%d:\n%!"
    l.loc_start.pos_lnum (l.loc_start.pos_cnum - l.loc_start.pos_bol);
  let dist_p1 = l.loc_start.pos_lnum - p1.Lexing.pos_lnum in
  let dist_p2 = p2.Lexing.pos_lnum - l.loc_end.pos_lnum in
  if dist_p1 > 1 && dist_p2 > 1 then
    let () = Printf.eprintf "  floating comment\n%!" in
    Float
  else if dist_p1 < dist_p2 then
    let () = Printf.eprintf "  fewer lines to prev\n%!" in
    Left
  else if dist_p1 > dist_p2 then
    let () = Printf.eprintf "  fewer lines to next\n%!" in
    Right
  else if p1.pos_lnum = p2.pos_lnum then
    let () = Printf.eprintf "  same line, defaulting to prev\n%!" in
    Left (* pfff *)
  else
    let prev_indent = C.get_line_indent (l.loc_start.pos_lnum - 1) in
    let indent = C.get_line_indent l.loc_start.pos_lnum in
    let next_indent = C.get_line_indent p2.pos_lnum in
    if prev_indent = next_indent then
      let () =
        Printf.eprintf
          "  same indent before(%d) and after(%d), defaulting to prev\n%!"
          prev_indent next_indent
      in
      Left (* pfff *)
    else
      let dist_p1 = abs (prev_indent - indent) in
      let dist_p2 = abs (next_indent - indent) in
      Printf.eprintf "  deciding based on indent:\n    \
                      prev: %d\n    \
                      cmt: %d  (%d < - > %d)\n    \
                      next: %d\n%!"
        prev_indent indent dist_p1 dist_p2 next_indent;
      if dist_p1 <= dist_p2 then
        Left
      else
        Right

let between_pos p1 p2 =
  let comments = C.between p1 p2 () in
  List.fold_right (fun ((_, l) as cmt) (left, float, right) ->
    let cmt = comment cmt in
    match decide_placement p1 p2 l with
    | Left  -> cmt :: left, float, right
    | Float -> left, cmt :: float, right
    | Right -> left, float, cmt :: right
  ) comments ([], [], [])

(* Returns the comments between two locations.
   These comments are separated in two lists based on whether we deem them to be
   closer to the first or to the second location. *)
let between l1 l2 =
  between_pos l1.loc_end l2.loc_start
