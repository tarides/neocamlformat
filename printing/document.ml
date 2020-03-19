include PPrint
open Source_parsing
open Location

let merge_locs l1 l2 =
  { Location.loc_start = l1.loc_start; loc_end = l2.loc_end }

let loc_between t1 t2 =
  { Location.loc_start = t1.loc.loc_end; loc_end = t2.loc.loc_start }

let comment (s, _) =
  !^"(*" ^^ arbitrary_string s ^^ !^"*)"

type comments =
  | No_comment
  | Attach_fst of document
  | Attach_snd of document

let comments_between_pos p1 p2 =
  match Source_parsing.Comments.between p1 p2 () with
  | [] -> No_comment
  | comments ->
    let doc = separate_map (break 1) comment comments in
    let fst = snd (List.hd comments) in
    let lst = snd (List.hd (List.rev comments)) in
    let dist_fst = p1.pos_cnum - fst.loc_start.pos_cnum in
    let dist_lst = p2.pos_cnum - lst.loc_start.pos_cnum in
    if dist_fst < dist_lst then
      Attach_fst doc
    else
      Attach_snd doc

let comments_between t1 t2 =
  comments_between_pos t1.loc.loc_end t2.loc.loc_start

type t = document loc

let empty ~loc =
  { txt = empty; loc }

let str s = { s with txt = string s.txt }

let arbitrary_string ~loc x =
  { txt = arbitrary_string x; loc }

let string ~loc x =
  { txt = string x; loc }

let char ~loc c =
  { txt = char c; loc }

let underscore ~loc : t=
  { txt = underscore; loc }

type doc_token =
  | Colon
  | Qmark
  | Equals
  | Coerce
  | In
  | When
  | Rarrow
  | Larrow
  | Do
  | Dot
  | Dotdot
  | Of
  | As
  | Cons
  | Pipe
  | With
  | Then
  | Else
  | Semi
  | To
  | Downto
  | Sharp
  | Colonequals
  | Plusequals
  | Star

let token_of_doc_token t : Source_parsing.Source_parser.token =
  match t with
  | Colon -> COLON
  | Qmark -> QUESTION
  | Equals -> EQUAL
  | Coerce -> COLONGREATER
  | In -> IN
  | When -> WHEN
  | Rarrow -> MINUSGREATER
  | Larrow -> LESSMINUS
  | Do -> DO
  | Dot -> DOT
  | Dotdot -> DOTDOT
  | Of -> OF
  | As -> AS
  | Cons -> COLONCOLON
  | Pipe -> BAR
  | With -> WITH
  | Then -> THEN
  | Else -> ELSE
  | Semi -> SEMI
  | To -> TO
  | Downto -> DOWNTO
  | Sharp -> HASH
  | Colonequals -> COLONEQUAL
  | Plusequals -> PLUSEQ
  | Star -> STAR

let string_of_token = function
  | Colon -> ":"
  | Qmark -> "?"
  | Equals -> "="
  | Coerce -> ":>"
  | In -> "in"
  | When -> "when"
  | Rarrow -> "->"
  | Larrow -> "<-"
  | Do -> "do"
  | Dot -> "."
  | Dotdot -> ".."
  | Of -> "of"
  | As -> "as"
  | Cons -> "::"
  | Pipe -> "|"
  | With -> "with"
  | Then -> "then"
  | Else -> "else"
  | Semi -> ";"
  | To -> "to"
  | Downto -> "downto"
  | Sharp -> "#"
  | Colonequals -> ":="
  | Plusequals -> "+="
  | Star -> "*"

let token ~loc t =
  string ~loc (string_of_token t)

let token_between x1 x2 tok =
  let loc =
    Source_parsing.Source.loc_of_token_between 
      ~start:x1.loc.loc_end
      ~stop:x2.loc.loc_start
      (token_of_doc_token tok)
  in
  token ~loc tok

(* FIXME: do I really want to keep this?
   Currently it's being used for:
   - class paths in core types: #A.t, which can contain
   comments: [#(*foo*)A.t]
   - ppat_types: #foo, which can contain comments [# (*foo*) foo]
   - the [if] token...
   - Pexp_newtypes
*)
let (++) doc t =
  { txt = doc ^^ t.txt
  ; loc = t.loc }

let angles (t : t) = { t with txt = angles t.txt }
let braces (t : t) = { t with txt = braces t.txt }
let brackets (t : t) = { t with txt = brackets t.txt }
let dquotes (t : t) = { t with txt = dquotes t.txt }
let parens (t : t) = { t with txt = parens t.txt }
let squotes (t : t) = { t with txt = squotes t.txt }

let enclose ~before ~after t =
  let txt = before ^^ t.txt ^^ after in
  { t with txt }

(* FIXME: sep is shit, remove. *)
let concat ?(sep=PPrint.empty) t1 t2 =
  let txt =
    match comments_between t1 t2 with
    | No_comment -> t1.txt ^^ sep ^^ t2.txt
    | Attach_fst cmts -> t1.txt ^/^ cmts ^^ sep ^^ t2.txt
    | Attach_snd cmts -> t1.txt ^^ sep ^^ cmts ^/^ t2.txt
  in
  { txt; loc = merge_locs t1.loc t2.loc }

let separate sep doc docs =
  match docs with
  | [] -> doc
  | _ -> List.fold_left (concat ~sep) doc docs

let separate_map sep ~f doc docs =
  separate sep (f doc) (List.map f docs)

let break_before ?(spaces=1) t =
  { t with txt = break spaces ^^ t.txt }

module Two_separated_parts = struct
  (** Degrades in the following way:
      {v
        Foo of bar

        Foo of
          bar

        Foo
          of
          bar
      v}
  *)
  let sep_with_first fst snd ~sep =
    let cmts_doc =
      match comments_between fst snd with
      | No_comment -> PPrint.empty
      | Attach_fst doc
      | Attach_snd doc -> doc ^^ break 1
    in
    let txt =
      group (
        group (fst.txt ^^ nest 2 (break 1 ^^ cmts_doc ^^ sep))
        ^^ nest 2 (break 1 ^^ snd.txt)
      )
    in
    { txt; loc = merge_locs fst.loc snd.loc}

  (** Degrades in the following way:
      {v
        Foo of bar

        Foo
          of bar

        Foo
          of
          bar
      v}
  *)
  let sep_with_second fst snd ~sep =
    let cmts_doc =
      match comments_between fst snd with
      | No_comment -> PPrint.empty
      | Attach_fst doc
      | Attach_snd doc -> doc ^^ break 1
    in
    let txt =
      group (
        fst.txt ^^
        nest 2 (break 1 ^^ cmts_doc ^^ group (sep ^/^ snd.txt))
      )
    in
    { txt; loc = merge_locs fst.loc snd.loc }
end

(* Wrapping PPrint combinators *)

let (^^) t1 t2 = concat t1 t2

let (^/^) t1 t2 = concat t1 t2 ~sep:(break 1)

let group t = { t with txt = group t.txt }

let nest n t = { t with txt = nest n t.txt }

let hang n t = { t with txt = hang n t.txt }

let optional ~loc f = function
  | None -> empty ~loc
  | Some d -> f d

let prefix ~indent ~spaces x y =
  group (x ^^ nest indent (break_before ~spaces y))

let infix ~indent ~spaces op x y =
  prefix ~indent ~spaces (concat x ~sep:PPrint.(blank spaces) op) y

let left_assoc_map ?sep ~f first rest =
  List.fold_left (fun t elt ->
    let elt = f elt in 
    match sep with
    | None -> t ^/^ elt
    | Some sep ->
      let sep = token_between t elt sep in
      t ^/^ group (sep ^/^ elt)
  ) (f first) rest

let flow_map sep f first rest =
  List.fold_left (fun t elt ->
    let elt = f elt in
    let sep = { txt = sep; loc = loc_between t elt } in
    t ^^ group (sep ^^ elt)
  ) (f first) rest

let flow sep first rest =
  flow_map sep (fun x -> x) first rest

module List_like = struct
  let docked_fields x xs =
    let fmt x = nest 2 (group (break_before x)) in
    List.fold_left
      (fun acc elt ->
          let elt = fmt elt in
          let semi = token_between acc elt Semi in
          group (acc ^^ semi) ^^ elt)
      (fmt x) 
      xs

  let docked ~left ~right x xs =
    let fields = docked_fields x xs in
    enclose ~before:left ~after:PPrint.(group (break 1 ^^ right))
      fields

  let fit_or_vertical_fields x xs =
    let fields = separate PPrint.(semi ^^ break 1) x xs in
    nest 2 (break_before fields)

  let fit_or_vertical ~left ~right x xs  =
    let fields = fit_or_vertical_fields x xs in
    enclose ~before:left ~after:PPrint.(break 1 ^^ right) fields

  let pp ~loc ~formatting ~left ~right = function
    | [] ->
      let cmts =
        match comments_between_pos loc.loc_start loc.loc_end with
        | No_comment -> PPrint.empty
        | Attach_fst doc | Attach_snd doc -> PPrint.(doc ^^ break 1)
      in
      { txt = PPrint.(left ^/^ cmts ^^ right); loc }
    | x :: xs ->
      match (formatting : Options.Wrappable.t) with
      | Wrap -> docked ~left ~right x xs
      | Fit_or_vertical -> fit_or_vertical ~left ~right x xs

  let pp_fields ~formatting x xs =
    match (formatting : Options.Wrappable.t) with
    | Wrap -> docked_fields x xs
    | Fit_or_vertical -> fit_or_vertical_fields x xs
end
