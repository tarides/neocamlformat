include PPrint
open Location

let merge_locs l1 l2 =
  { Location.loc_start = l1.loc_start; loc_end = l2.loc_end;
    loc_ghost = false (* useless *) }

let loc_between t1 t2 =
  { Location.loc_start = t1.loc.loc_end; loc_end = t2.loc.loc_start;
    loc_ghost = true (* useless *) }

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

let token_between x1 x2 tok =
  let loc = loc_between x1 x2 in
  string ~loc tok

(* FIXME: do I really want to keep this?
   Currently it's being used for:
   - class paths in core types: #A.t, which can contain
   comments: #(*foo*)A.t
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

let concat ?(sep=PPrint.empty) t1 t2 =
  { txt = t1.txt ^^ sep ^^ t2.txt
  ; loc = merge_locs t1.loc t2.loc }

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
    let txt =
      group (
        group (fst.txt ^^ nest 2 (break 1 ^^ sep))
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
    let txt =
      group (
        fst.txt ^^
        nest 2 (break 1 ^^ group (sep ^/^ snd.txt))
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

let prefix ~indent ~spaces x y =
  group (x ^^ nest indent (break_before ~spaces y))

let left_assoc_map ~sep ~f first rest =
  List.fold_left (fun t elt ->
    let elt = f elt in 
    let sep = { txt = sep; loc = loc_between t elt } in
    t ^/^ group (sep ^^ elt)
  ) (f first) rest

module List_like = struct
  let docked ~left ~right x xs =
    let fmt x = nest 2 (group (break_before x)) in
    let fields =
      List.fold_left
        (fun acc elt ->
           let elt = fmt elt in
           let semi = token_between acc elt ";" in
           group (acc ^^ semi) ^^ fmt elt)
        (fmt x) 
        xs
    in
    let txt =
      let open PPrint in
      left ^^ fields.txt ^^ group (break 1 ^^ right)
    in
    { txt; loc = fields.loc }

  let fit_or_vertical ~left ~right x xs  =
    let fields = separate PPrint.(semi ^^ break 1) x xs in
    let txt = PPrint.(left ^^ nest 2 ( break 1 ^^ fields.txt) ^/^ right) in
    { txt; loc = fields.loc }

  let pp ~loc ~formatting ~left ~right = function
    | [] -> { txt = PPrint.(left ^^ right); loc }
    | x :: xs ->
      match (formatting : Options.Wrappable.t) with
      | Wrap -> docked ~left ~right x xs
      | Fit_or_vertical -> fit_or_vertical ~left ~right x xs
end
