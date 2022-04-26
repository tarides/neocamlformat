open Source_parsing
open Location

let merge_locs l1 l2 =
  { Location.loc_start = l1.loc_start; loc_end = l2.loc_end }

let loc_between t1 t2 =
  { Location.loc_start = t1.loc.loc_end; loc_end = t2.loc.loc_start }

type comments =
  | No_comment
  | Attach_fst of PPrint.document
  | Attach_snd of PPrint.document

type ws_kind =
  | Space
  | Hardline

type doc =
  | Empty
  | WS of PPrint.document
  | Located of PPrint.document loc

type t = {
  doc: doc;
  left: border_info;
  right: border_info;
}

and border_info = {
  nest: int;
  ws: ws_kind option;
}

let map_loc f { txt; loc } = { txt = f txt; loc }

let empty =
  let bi = { nest = 0; ws = None } in
  { doc = Empty; left = bi; right = bi }

let string ~loc s =
  let bi = { nest = 0; ws = None } in
  { doc = Located (mkloc (PPrint.string s) loc); left = bi; right = bi }

let str (s : string loc) =
  let bi = { nest = 0; ws = None } in
  { doc = Located (map_loc PPrint.string s); left = bi; right = bi }

let hardline =
  let bi = { nest = 0; ws = Some Hardline } in
  { doc = WS PPrint.hardline; left = bi; right = bi }

let break n =
  let bi = { nest = 0; ws = Some Space } in
  { doc = WS (PPrint.break n); left = bi; right = bi }

let blank n =
  let bi = { nest = 0; ws = Some Space } in
  { doc = WS (PPrint.blank n); left = bi; right = bi }

let space = blank 1

let repeat n d =
  let rec aux n doc =
    if n = 1
    then doc
    else PPrint.(doc ^^ aux (n - 1) doc)
  in
  match d.doc with
  | Empty -> d
  | WS doc -> { d with doc = WS (aux n doc) }
  | Located doc -> { d with doc = Located (map_loc (aux n) doc) }

let twice = repeat 2

let nest nb t =
  let left = { t.left with nest = t.left.nest + nb } in
  let right = { t.right with nest = t.right.nest + nb } in
  match t.doc with
  | Empty -> t
  | WS doc -> { doc = WS (PPrint.nest nb doc); left; right }
  | Located doc -> { doc = Located (map_loc (PPrint.nest nb) doc); left; right }

let group t =
  match t.doc with
  | Empty -> t
  | WS doc -> { t with doc = WS (PPrint.group doc) }
  | Located doc -> { t with doc = Located (map_loc PPrint.group doc) }

let hang nb t =
  match t.doc with
  | Empty -> t
  | WS doc -> { t with doc = WS (PPrint.hang nb doc) }
  | Located doc -> { t with doc = Located (map_loc (PPrint.hang nb) doc) }

let docstring ~loc s (l : Location.t) =
  (* [l] is the location of [s], without the "(**" "*)". *)
  let loc_start = { l.loc_start with  pos_cnum = l.loc_start.pos_cnum + 3 } in
  let l = { l with  loc_start } in
  let doc =
    let open PPrint in
    !^"(**" ^^ Verbatim.pp_string ~comment:true ~adjust_indent:l s ^^ !^"*)"
  in
  let bi = { nest = 0; ws = None } in
  { doc = Located { txt = doc; loc }; left = bi; right = bi }

let arbitrary_string ~loc x =
  let bi = { nest = 0; ws = None } in
  { doc = Located { txt = PPrint.arbitrary_string x; loc };
    left = bi; right = bi }

let quoted_string ?adjust_indent ~loc s =
  let adjust_indent = Option.map (Fun.const loc) adjust_indent in
  let bi = { nest = 0; ws = None } in
  let txt = Verbatim.pp_string ?adjust_indent s in
  { doc = Located { txt; loc }; left = bi; right = bi }

let char ~loc c =
  let bi = { nest = 0; ws = None } in
  let c = PPrint.(squotes @@ string @@ Char.escaped c) in
  { doc = Located (mkloc c loc); left = bi; right = bi }

let underscore ~loc : t =
  let bi = { nest = 0; ws = None } in
  { doc = Located (mkloc PPrint.underscore loc); left = bi; right = bi }

let concat_located (d1, right) (d2, left) =
  let final_loc = merge_locs d1.loc d2.loc in
  let open PPrint in
  final_loc,
  match Comment.between d1.loc d2.loc with
  | [], [] -> d1.txt ^^ d2.txt
  | left_cmts, right_cmts ->
    let d1 = d1.txt in
    let d2 = d2.txt in
    (* FIXME: in each group, comments are separated by one space. 
       Is this the behaviour we want?

       For instance, in the absence of any ws between d1 and d2, we probably
       want to [break 0] between the comments. *)
    (* FIXME: what about the loc of comments themselves that I am dropping here.
       Do we want to keep it? Is it useful? *)
    let left_cmts  = separate_map (break 1) (fun x -> x.txt) left_cmts in
    let right_cmts = separate_map (break 1) (fun x -> x.txt) right_cmts in
    (* FIXME: Perhaps I don't need all these hardlines in there.
       The presence of them anywhere, is going to turn all the break into
       hardlines, in the absence of grouping. *)
    (* FIXME: add groups? it's not working well, we'd want to push comments into
       existing groups … but that's expensive and probably not worth it? *)
    let layout indent ?before ?after cmts =
      if cmts == empty then
        empty
      else 
        let break i = if i = -1 then hardline else break i in
        nest indent
          begin match before, after with
          | None, None -> cmts
          | Some i, None -> break i ^^ cmts
          | None, Some j -> cmts ^^ break j
          | Some i, Some j -> break i ^^ cmts ^^ break j
          end
    in
    match right.ws, left.ws with
    | None, None ->
      d1
      ^^ layout right.nest ~before:0 left_cmts
      ^^ layout left.nest ~before:0 right_cmts ~after:0
      ^^ d2
    | Some Space, Some Space ->
      d1
      ^^ layout right.nest left_cmts
      ^^ layout left.nest ~before:1 right_cmts
      ^^ d2
    | Some Hardline, Some Hardline ->
      d1
      ^^ layout right.nest left_cmts
      ^^ layout left.nest ~before:(-1) right_cmts
      ^^ d2
    | Some Hardline, Some _
    | Some _, Some Hardline ->
      d1
      ^^ layout right.nest left_cmts
      ^^ layout left.nest ~before:(-1) right_cmts
      ^^ d2
    | Some Hardline, None ->
      d1
      ^^ layout right.nest left_cmts
      ^^ layout left.nest ~before:(-1) right_cmts ~after:0
      ^^ d2
    | None, Some Hardline ->
      d1
      ^^ layout right.nest ~before:0 left_cmts
      ^^ layout left.nest ~before:(-1) right_cmts
      ^^ d2
    | Some Space, None ->
      d1
      ^^ layout right.nest left_cmts
      ^^ layout left.nest ~before:1 right_cmts ~after:0
      ^^ d2
    | None, Some Space ->
      d1
      ^^ layout right.nest ~before:0 left_cmts
      ^^ layout left.nest ~before:1 right_cmts
      ^^ d2

let (^^) t1 t2 =
  match t1.doc, t2.doc with
  | Empty, _ -> t2
  | _, Empty -> t1
  | WS d1, WS d2 ->
    let doc = WS PPrint.(d1 ^^ d2) in
    { doc; left = t1.left; right = t2.right }
  | Located d1, Located d2 ->
    let loc, txt = concat_located (d1, t1.right) (d2, t2.left) in
    let doc = Located { txt; loc } in
    { doc; left = t1.left; right = t2.right }
  | WS d1, Located d2 ->
    let d = PPrint.(d1 ^^ d2.txt) in
    let doc = Located { txt = d; loc = d2.loc } in
    { doc; left = t1.left; right = t2.right }
  | Located d1, WS d2 ->
    let d = PPrint.(d1.txt ^^ d2) in
    let doc = Located { txt = d; loc = d1.loc } in
    { doc; left = t1.left; right = t2.right }

(* FIXME: do I really want to keep this?
   Currently it's being used for:
   - class paths in core types: #A.t, which can contain
   comments: [#(*foo*)A.t]
   - ppat_types: #foo, which can contain comments [# (*foo*) foo]
   - the [if] token...
   - Pexp_newtypes
*)

(* Should be used only to insert tokens not necessarily present in the source *)
(* FIXME: but if they are present, we'd want to reuse them, to get accurate
   locations and comment placement. *)
let (++) doc t =
  match t.doc with
  | Empty
  | WS _ -> invalid_arg "Document.(++)"
  | Located d ->
    { t with doc = Located (map_loc (fun t -> PPrint.(^^) doc t) d) }
    (*
let (+++) t doc =
  { t with doc = t.doc ^^ doc }
*)
let suffix ~after:t c =
  match t.doc with
  | Empty 
  | WS _ -> invalid_arg "Document.suffix"
  | Located doc ->
    { t with doc = Located (map_loc (fun d -> PPrint.(d ^^ char c)) doc) }
    (*
let angles (t : t) =
  { t with  doc = angles t.doc }
let braces (t : t) =
  { t with  doc = braces t.doc }
let brackets (t : t) =
  { t with  doc = brackets t.doc }
let dquotes (t : t) =
  { t with  doc = dquotes t.doc }
let parens (t : t) =
  { t with  doc = parens t.doc }
let squotes (t : t) =
  { t with  doc = squotes t.doc }

let enclose ~before ~after t =
  let doc = before ^^ t.doc ^^ after in
  { t with  doc }
*)

let collate_toplevel_items lst =
  let rec insert_blanks = function
    | [] -> invalid_arg "collate_toplevel_items"
    | [ x ] -> [ `Doc x ]
    | x1 :: x2 :: rest ->
      let requirement x =
        match x.doc with
        | WS doc
        | Located { txt = doc; _ } -> PPrint.requirement doc
        | Empty -> 0
      in
      `Doc x1 ::
        (if
          requirement x1 > !Options.width || requirement x2 > !Options.width
        then
          `Twice
        else
          `Once) ::
          insert_blanks (x2 :: rest)
  in
  let rec join = function
    | `Doc x1 :: `Twice :: `Doc x2 :: rest ->
      let joined = x1 ^^ twice hardline ^^ x2 in
      join (`Doc joined :: rest)
    | `Doc x1 :: `Once :: `Doc x2 :: rest ->
      let joined = x1 ^^ hardline ^^ x2 in
      join (`Doc joined :: rest)
    | otherwise -> otherwise
  in
  match join (insert_blanks lst) with
  | [ `Doc x ] -> x
  | _ -> assert false

let separate sep doc docs =
  match docs with
  | [] -> doc
  | _ -> List.fold_left (fun d1 d2 -> d1 ^^ sep ^^ d2) doc docs

let separate_map sep ~f doc docs =
  separate sep (f doc) (List.map f docs)

(* Wrapping PPrint combinators *)
let (^/^) t1 t2 = t1 ^^ break 1 ^^ t2
let (^//^) t1 t2 = t1 ^^ hardline ^^ t2

let ifflat t1 t2 =
  (* FIXME: what about border infos? are they necessarily the same? :/ *)
  match t1.doc, t2.doc with
  | Empty, Empty -> t1
  | WS d1, WS d2 -> { t1 with doc = WS (PPrint.ifflat d1 d2) }
  | WS flat, Empty -> { t1 with doc = WS PPrint.(ifflat flat empty) }
  | Empty, WS nonflat -> { t1 with doc = WS PPrint.(ifflat empty nonflat) }
  | Located d1, Located d2 ->
    let txt = PPrint.ifflat d1.txt d2.txt in
    (* TODO: assert same locs *)
    { t1 with doc = Located { txt; loc = d1.loc } }
  | Located { txt = flat; loc }, WS nonflat
  | WS flat, Located { txt = nonflat; loc } ->
    let txt = PPrint.ifflat flat nonflat in
    { t1 with doc = Located { txt; loc } }
  | Located { txt = flat; loc }, Empty ->
    let txt = PPrint.(ifflat flat empty) in
    { t1 with doc = Located { txt; loc } }
  | Empty, Located { txt = nonflat; loc } ->
    let txt = PPrint.(ifflat empty nonflat) in
    { t1 with doc = Located { txt; loc } }

(*
let optional ~loc f = function
  | None -> empty ~loc
  | Some d -> f d
   *)

let prefix ~indent ~spaces x y =
  group (x ^^ nest indent (break spaces ^^ y))

let infix ~indent ~spaces op x y =
  prefix ~indent ~spaces (x ^^ blank spaces ^^ op) y


let flow_map sep f first rest =
  List.fold_left (fun t elt ->
    let elt = f elt in
    t ^^ group (sep ^^ elt)
  ) (f first) rest

let flow sep first rest =
  flow_map sep (fun x -> x) first rest

let fmt_comments before = function
  | [] -> empty
  | c :: cs ->
    let bi = { nest = 0; ws = None } in
    let cmt x = { doc = Located (Comment.comment x); left = bi; right = bi } in
    let cmts = separate_map hardline ~f:cmt c cs in
    if before then cmts ^^ hardline else hardline ^^ cmts

let attach_surrounding_comments doc =
  match doc.doc with
  | Empty -> fmt_comments true (Comments.get ())
  | Located { loc; _ } ->
    let before = Comments.before loc.loc_start |> fmt_comments true in
    let after = Comments.after loc.loc_end |> fmt_comments false in
    before ^^ doc ^^ after
  | WS _ -> invalid_arg "Document.attach_surrounding_comments"

let to_pprint t =
  match t.doc with
  | Empty -> PPrint.empty
  | WS doc -> doc
  | Located { txt; _ } -> txt
