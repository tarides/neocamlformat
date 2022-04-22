let column pos = pos.Lexing.pos_cnum - pos.pos_bol

let compare_pos p1 p2 =
  match compare p1.Lexing.pos_lnum p2.Lexing.pos_lnum with
  | 0 -> compare (column p1) (column p2)
  | n -> n

let is_docstring s =
  match s with
  | "" -> false
  | "*" ->
     (* FIXME ? this can be either (**) or (***), one can look at
        locations to compuate the width *)
     false
  | _ ->
     String.get s 0 = '*'
     && String.length s >= 2
     && String.get s 1 <> '*'

type t =
  | Null
  | Cell of {
      loc: Location.t;
      txt: string;
      mutable prev: t;
      mutable next: t;
    }

let rec iter ~f = function
  | Null -> ()
  | Cell { loc; txt; next; _ } ->
    f loc txt;
    iter ~f next

let set_next ~next = function
  | Null -> ()
  | Cell cell -> cell.next <- next

let set_prev ~prev = function
  | Null -> ()
  | Cell cell -> cell.prev <- prev

let keep_first fst snd = if fst = Null then snd else fst

let add_before ~prev ~next txt loc =
  let cell = Cell { loc; txt; prev; next } in
  set_next prev ~next:cell;
  set_prev ~prev:cell next;
  keep_first prev cell

let rec insert_comment prev txt loc = function
  | Null -> add_before ~prev ~next:Null txt loc
  | Cell t as cell ->
    let start = compare_pos loc.loc_start t.loc.loc_start in
    if start < 0 then
      add_before ~prev txt loc ~next:cell
    else
      let _ = insert_comment cell txt loc t.next in
      keep_first prev cell

let keep s = not (is_docstring s)

let populate rest =
      List.fold_left (fun lst (txt, loc) ->
        if keep txt then
          insert_comment Null txt loc lst
        else
          lst
      ) Null rest

let comments = ref Null

let init l =
  comments := populate l

let fetch accept pos =
  let rec aux = function
    | Null -> [], Null
    | Cell { txt; loc; prev; next } as cell ->
      let relative_pos = compare_pos pos loc.loc_start in
      if not (accept relative_pos) then
        let (comments, _) = aux next in
        comments, cell
      else
        let (comments, next) = aux next in
        set_next prev ~next;
        set_prev ~prev next;
        (txt, loc) :: comments, next
  in
  let (cmts, remaining) = aux !comments in
  comments := remaining;
  cmts

let between pos1 pos2 () =
  let rec aux ~stitch = function
    | Null -> [], Null
    | Cell { txt; loc; prev; next } as cell ->
      if compare_pos pos1 loc.loc_start > 0 then
        (* Comment before pos1 *)
        let (comments, _) = aux ~stitch:true next in
        comments, cell
      else if compare_pos loc.loc_start pos2 > 0 then
        (* Comment after pos2 *)
        [], cell
      else
        (* Comment in range *)
        let (comments, next) = aux ~stitch:false next in
        let comments = (txt, loc) :: comments in
        if stitch then (
          set_next prev ~next;
          set_prev ~prev next;
          comments, keep_first prev next
        ) else (
          comments, next
        )
  in
  let (in_range, remaining) = aux ~stitch:true !comments in
  comments := remaining;
  in_range

let before = fetch (fun relative_pos -> relative_pos >= 0)
let after = fetch (fun relative_pos -> relative_pos <= 0)

let get () =
  let rec aux = function
    | Null -> []
    | Cell { loc; txt; prev = _; next } -> (txt, loc) :: aux next
  in
  let lst = aux !comments in
  comments := Null;
  lst

let report_remaining () =
  iter !comments ~f:(fun l _ -> Format.eprintf "- %a\n%!" Location.print_loc l)
