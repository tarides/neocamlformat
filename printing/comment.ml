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

let between_pos p1 p2 =
  let comments = Source_parsing.Comments.between p1 p2 () in
  List.partition_map (fun ((_, l) as cmt) ->
    let cmt = comment cmt in
    let dist_p1 = l.loc_start.pos_lnum - p1.pos_lnum in
    let dist_p2 = p2.pos_lnum - l.loc_end.pos_lnum in
    if dist_p1 < dist_p2 then
      Left cmt
    else if dist_p1 > dist_p2 then
      Right cmt
    else
      let dist_p1 = l.loc_start.pos_cnum - p1.pos_cnum in
      let dist_p2 = p2.pos_cnum - l.loc_end.pos_cnum in
      if dist_p1 < dist_p2 then
        Left cmt
      else
        Right cmt
  ) comments

(* Returns the comments between two locations.
   These comments are separated in two lists based on whether we deem them to be
   closer to the first or to the second location. *)
let between l1 l2 =
  between_pos l1.loc_end l2.loc_start
