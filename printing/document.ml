include PPrint
open Source_parsing
open Location

let can_shift_lines comment init_col s =
  let rec all_blank ~from ~to_ =
    from = to_ ||
    (String.get s from = ' ' && all_blank ~from:(from + 1) ~to_)
  in
  let rec aux i =
    match String.index_from_opt s i '\n' with
    | None -> true
    | Some j ->
      let from = j + 1 in
      let to_ = from + init_col in
      begin try
        (comment || String.get s (j - 1) = '\\') && all_blank ~from ~to_
      with Invalid_argument _ (* out of bounds *) ->
        false
      end && aux to_
  in
  aux 0

class verbatim_string ?(comment=false) ?adjust_indent s : PPrint.custom =
  let req = if String.contains s '\n' then infinity else String.length s in
  let init_col =
    Option.bind adjust_indent (fun (loc:Location.t) ->
      let init_col = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
      if can_shift_lines comment init_col s
      then Some init_col
      else None
    )
  in
  object
    method requirement = req

    method pretty output state _ flattening =
      let shift =
        match init_col with
        | None -> `Done
        | Some init_col ->
          let shift = state.column - init_col in
          if shift = 0 then
            `Done
          else if shift > 0 then
            `Positive (shift, String.init shift (Fun.const ' '))
          else
            `Negative (abs shift)
      in
      let rec aux i =
        match String.index_from_opt s i '\n' with
        | Some j ->
          assert (not flattening);
          if j - i > 0 then output#substring s i (j - i);
          output#char '\n';
          state.line <- state.line + 1;
          let j =
            match shift with
            | `Done ->
              state.column <- 0;
              j
            | `Positive (len, str) ->
              output#substring str 0 len;
              state.column <- len;
              j
            | `Negative i ->
              state.column <- 0;
              j + i
          in
          aux (j + 1)
        | None ->
          let len = String.length s - i in
          output#substring s i len;
          state.column <- state.column + len;
      in
      aux 0

    method compact output =
      assert (req != infinity);
      output#substring s 0 req
  end

let pp_verbatim_string ?comment ?adjust_indent s =
  custom (new verbatim_string ?comment ?adjust_indent s)

let merge_locs l1 l2 =
  { Location.loc_start = l1.loc_start; loc_end = l2.loc_end }

let loc_between t1 t2 =
  { Location.loc_start = t1.loc.loc_end; loc_end = t2.loc.loc_start }

let comment (s, (l : Location.t)) =
  let loc_start =
    { l.loc_start with pos_cnum = l.loc_start.pos_cnum + 2 }
  in
  let l = { l with loc_start } in
  !^"(*" ^^ pp_verbatim_string ~comment:true ~adjust_indent:l s ^^ !^"*)"

let docstring s (l : Location.t) =
  let loc_start =
    { l.loc_start with pos_cnum = l.loc_start.pos_cnum + 3 }
  in
  let l = { l with loc_start } in
  !^"(**" ^^ pp_verbatim_string ~comment:true ~adjust_indent:l s ^^ !^"*)"

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
    let dist_lst = p2.pos_cnum - lst.loc_end.pos_cnum in
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

let quoted_string ?adjust_indent ~loc s =
  let adjust_indent = Option.map (Fun.const loc) adjust_indent in
  let txt = pp_verbatim_string ?adjust_indent s in
  { txt; loc }

let string ~loc x =
  { txt = string x; loc }

let char ~loc c =
  { txt = char c; loc }

let underscore ~loc : t=
  { txt = underscore; loc }

let token ~loc t =
  string ~loc (Source_parsing.Source.print_tok t)

let token_between_locs start stop tok =
  let loc =
    Source_parsing.Source.loc_of_token_between ~start ~stop
      tok
  in
  token ~loc tok

let token_between x1 x2 tok =
  token_between_locs x1.loc.loc_end x2.loc.loc_start tok

let token_before ~start doc tok =
  token_between_locs start doc.loc.loc_start tok

let token_after ~stop doc tok =
  token_between_locs doc.loc.loc_end stop tok

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

let (+++) t doc =
  { txt = t.txt ^^ doc
  ; loc = t.loc }

let suffix ~after:t doc =
  { txt = t.txt ^^ doc
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

let fmt_comments before = function
  | [] -> PPrint.empty
  | comments ->
    let cmts = separate_map hardline comment comments in
    if before then
      cmts ^^ hardline
    else
      hardline ^^ cmts

let attach_surrounding_comments doc =
  let before =
    Comments.before doc.loc.loc_start
    |> fmt_comments true
  in
  let after =
    Comments.after doc.loc.loc_end
    |> fmt_comments false
  in
  before ^^ doc.txt ^^ after

(* FIXME: sep is shit, remove. *)
let merge_possibly_swapped ?(sep=PPrint.empty) d1 d2 =
  let t1, t2 = if Location.ends_before d1.loc d2.loc then d1, d2 else d2, d1 in
  let txt =
    match comments_between t1 t2 with
    | No_comment -> d1.txt ^^ sep ^^ d2.txt
    | Attach_fst cmts -> d1.txt ^/^ cmts ^^ sep ^^ d2.txt
    | Attach_snd cmts -> d1.txt ^^ sep ^^ cmts ^/^ d2.txt
  in
  { txt; loc = merge_locs t1.loc t2.loc }

(* FIXME: sep is shit, remove. *)
let concat ?(sep=PPrint.empty) ?(indent=0) t1 t2 =
  let txt =
    match comments_between t1 t2 with
    | No_comment -> t1.txt ^^ sep ^^ t2.txt
    | Attach_fst cmts -> t1.txt ^^ nest indent (break 1 ^^ cmts) ^^ sep ^^ t2.txt
    | Attach_snd cmts -> t1.txt ^^ sep ^^ nest indent cmts ^/^ t2.txt
  in
  { txt; loc = merge_locs t1.loc t2.loc }

let collate_toplevel_items lst =
  let rec insert_blanks = function
    | [] -> invalid_arg "collate_toplevel_items"
    | [ x ] -> [ `Doc x ]
    | x1 :: x2 :: rest ->
      `Doc x1
      :: (if requirement x1.txt > 80 (* TODO: width in config *)
          || requirement x2.txt > 80 (* TODO: width in config *)
          then `Twice
          else `Once)
      :: insert_blanks (x2 :: rest)
  in
  let rec join = function
    | `Doc x1 :: `Twice :: `Doc x2 :: rest ->
      let joined = concat x1 x2 ~sep:(twice hardline) in
      join (`Doc joined :: rest)
    | `Doc x1 :: `Once :: `Doc x2 :: rest ->
      let joined = concat x1 x2 ~sep:hardline in
      join (`Doc joined :: rest)
    | otherwise ->
      otherwise
  in
  match join (insert_blanks lst) with
  | [ `Doc x ] -> x
  | _ -> assert false

let separate sep doc docs =
  match docs with
  | [] -> doc
  | _ -> List.fold_left (concat ~sep) doc docs

let separate_map sep ~f doc docs =
  separate sep (f doc) (List.map f docs)

let break_after ?(spaces=1) t =
  { t with txt = t.txt ^^ break spaces }

let break_before ?(spaces=1) t =
  { t with txt = break spaces ^^ t.txt }

(* Wrapping PPrint combinators *)

let (^^) t1 t2 = concat t1 t2

let (^/^) t1 t2 = concat t1 t2 ~sep:(break 1)

let ifflat t1 t2 =
  (* TODO: assert same locs *)
  { t1 with txt = ifflat t1.txt t2.txt }

let group t = { t with txt = group t.txt }

let nest n t = { t with txt = nest n t.txt }

let hang n t = { t with txt = hang n t.txt }

let optional ~loc f = function
  | None -> empty ~loc
  | Some d -> f d

let prefix ~indent ~spaces x y =
  group (concat ~indent x (nest indent (break_before ~spaces y)))

let infix ~indent ~spaces op x y =
  prefix ~indent ~spaces (concat x ~sep:PPrint.(blank spaces) op) y

let left_assoc_map ?sep ~f first rest =
  List.fold_left (fun t elt ->
    let elt = f elt in
    match sep with
    | None -> t ^^ group (break_before elt)
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
    let sep = token_between fst snd sep in
    prefix ~indent:2 ~spaces:1
      (group (fst ^^ nest 2 (break_before sep)))
      snd

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
    let sep = token_between fst snd sep in
    prefix ~indent:2 ~spaces:1
      fst (group (sep ^/^ snd))
end

module Enclosed_separated = struct
  type raw = t * t list

  type element = {
    doc: t;
    has_semi: bool;
  }

  module Wrapped : sig
    val pp_fields: raw -> raw list -> t

    val pp : left:document -> right:document -> raw -> raw list -> t
  end = struct
    let ( * ) before after =
      { after with doc = before ^^ after.doc }

    let fmt (x, attrs) =
      let doc = nest 2 (group (break_before x)) in
      match attrs with
      | [] -> { doc; has_semi = false }
      | x :: xs ->
        let attrs = group (separate (PPrint.break 0) x xs) in
        { doc = group (suffix ~after:doc PPrint.semi ^/^ attrs)
        ; has_semi = true }

    let pp_fields x xs =
      let res =
        List.fold_left
          (fun { doc = acc; has_semi } elt ->
             let elt = fmt elt in
             if has_semi then
               acc * elt
             else
               let semi = token_between acc elt.doc SEMI in
               group (acc ^^ semi) * elt)
          (fmt x)
          xs
      in
      res.doc

    let pp ~left ~right x xs =
      let fields = pp_fields x xs in
      enclose ~before:left ~after:PPrint.(group (break 1 ^^ right))
        fields
  end

  module Fit_or_vertical : sig
    val pp_fields: raw -> raw list -> t

    val pp : left:document -> right:document -> raw -> raw list -> t
  end = struct
    let fmt (doc, attrs) =
      match attrs with
      | [] -> { doc = group doc; has_semi = false }
      | x :: xs ->
        let attrs = separate (PPrint.break 0) x xs in
        { doc = group (group (suffix ~after:doc semi) ^/^ attrs)
        ; has_semi = true }

    let rec pp_fields_aux = function
      | [] -> assert false
      | [ x ] -> (fmt x).doc
      | x :: ((_ :: _) as rest) ->
        let elt = fmt x in
        if elt.has_semi then
          elt.doc ^^ break_before (pp_fields_aux rest)
        else
          let rest = pp_fields_aux rest in
          let semi = token_between elt.doc rest SEMI in
          group (elt.doc ^^ semi) ^^ break_before rest

    let pp_fields x xs =
      let fields = pp_fields_aux (x :: xs) in
      nest 2 (break_before fields)

    let pp ~left ~right x xs  =
      let fields = pp_fields x xs in
      enclose ~before:left ~after:PPrint.(break 1 ^^ right) fields
  end

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
      | Wrap -> Wrapped.pp ~left ~right x xs
      | Fit_or_vertical -> Fit_or_vertical.pp ~left ~right x xs

  let pp_fields ~formatting x xs =
    match (formatting : Options.Wrappable.t) with
    | Wrap -> Wrapped.pp_fields x xs
    | Fit_or_vertical -> Fit_or_vertical.pp_fields x xs
end

module List_like = struct

  let pp ~loc ~formatting ~left ~right elts =
    let elts = List.map (fun x -> (x, [])) elts in
    Enclosed_separated.pp ~loc ~formatting ~left ~right elts

  let pp_fields ~formatting x xs =
    Enclosed_separated.pp_fields ~formatting
      (x, [])
      (List.map (fun x -> (x, [])) xs)
end

module Record_like = Enclosed_separated
