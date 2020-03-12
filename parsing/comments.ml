(* All this is inefficient… but there should be many comments :) *)

type t = unit

let comments = ref []

let init () =
  comments :=
    List.filter (fun (s, _) -> s <> "" && s.[0] <> '*')
      (Source_lexer.comments ())

let column pos = pos.Lexing.pos_cnum - pos.pos_bol

let compare_pos p1 p2 =
  match compare p1.Lexing.pos_lnum p2.Lexing.pos_lnum with
  | 0 -> compare (column p1) (column p2)
  | n -> n

let between pos1 pos2 () =
  let yes, no =
    List.fold_right (fun (s, l as elt) (yes, no) ->
        let start = compare_pos pos1 l.Location.loc_start in
        if start > 0 then
          (yes, elt :: no)
        else
          let stop = compare_pos l.loc_end pos2 in
          if stop > 0 then
            (yes, elt :: no)
          else
            (s :: yes, no)
    ) !comments ([], [])
  in
  comments := no;
  yes
