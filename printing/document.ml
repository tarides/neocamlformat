include PPrint
open Location

let merge_locs l1 l2 =
  { Location.loc_start = l1.loc_start; loc_end = l2.loc_end;
    loc_ghost = false (* useless *) }

let loc_between t1 t2 =
  { Location.loc_start = t1.loc.loc_end; loc_end = t2.loc.loc_start;
    loc_ghost = true (* useless *) }

type t = document loc

let str s = { s with txt = string s.txt }

let arbitrary_string ~loc x =
  { txt = arbitrary_string x; loc }

let string ~loc x =
  { txt = string x; loc }

let char ~loc c =
  { txt = char c; loc }

let underscore ~loc : t=
  { txt = underscore; loc }

let parens (t : t) = { t with txt = parens t.txt }
let braces (t : t) = { t with txt = braces t.txt }
let squotes (t : t) = { t with txt = squotes t.txt }
let dquotes (t : t) = { t with txt = dquotes t.txt }
let brackets (t : t) = { t with txt = brackets t.txt }

let enclose ~before ~after t =
  let txt = before ^^ t.txt ^^ after in
  { t with txt }

let concat ?(sep=empty) t1 t2 =
  { txt = t1.txt ^^ sep ^^ t2.txt
  ; loc = merge_locs t1.loc t2.loc }

let left_assoc_map ~sep ~f first rest =
  List.fold_left (fun t elt ->
    let elt = f elt in 
    let txt = t.txt ^/^ group (sep ^^ elt.txt) in
    { txt ; loc = merge_locs t.loc elt.loc }
  ) (f first) rest

let break_before t =
  { t with txt = break 1 ^^ t.txt }

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
