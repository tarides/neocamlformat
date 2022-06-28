open Document
open Source_parsing

module Two_separated_parts = struct(* FIXME: does sep_with_first make sense?

     Shouldn't it be just [nest 2 (flow (break 1) x [ xs ])] ?
     which would go to
     {v
       Foo
         of bar
     v}

     before going to

     {v
       Foo
         of
         bar
     v} *)

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
    let sep = Token.pp ~after:fst ~before:snd sep in
    prefix ~indent:2 ~spaces:1 (group (fst ^^ nest 2 (break 1 ^^ sep))) snd

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
    let sep = Token.pp ~after:fst ~before:snd sep in
    prefix ~indent:2 ~spaces:1 fst (group (sep ^/^ snd))
end


module Enclosed_separated = struct
  type raw =
    t * t list
  type element =
    {
      doc : t;
      has_semi : bool
    }

  module Wrapped : sig
    val pp_fields : raw -> raw list -> t

    val pp
      :  loc:Location.t
      -> left:Parser.token
      -> right:Parser.token
      -> raw
      -> raw list
      -> t
  end = struct
    let rec insert_semi : element list -> t list = function
      | [] -> assert false
      | [ x ] -> [ x.doc ]
      | x1 :: (x2 :: _ as xs) ->
        if x1.has_semi then
          x1.doc :: insert_semi xs
        else
          let semi = Token.pp ~after:x1.doc ~before:x2.doc SEMI in
          group (x1.doc ^^ semi) :: insert_semi xs

    let fmt (doc, attrs) =
      match attrs with
      | [] -> { doc; has_semi = false }
      | x :: xs ->
        let attrs = group (separate (break 0) x xs) in
        {
          doc = prefix ~indent:2 ~spaces:1 (suffix ~after:doc ';') attrs;
          has_semi = true
        }

    let rec collate ?(first=false) = function
      | [] -> assert false
      | [ x ] -> (* last one! *)
        if first then x else group (break 0 ^^ x)
      | x :: xs ->
        let x =
          if first then
            group (x ^^ break 1)
          else
            group (break 0 ^^ x ^^ break 1)
        in
        x ^^ collate xs

    let pp_fields x xs =
      List.map fmt (x :: xs) |> insert_semi |> collate ~first:true

    let pp ~loc ~left ~right x xs =
      let fields = pp_fields x xs in
      let left = Token.pp ~inside:loc ~before:fields left in
      let right = Token.pp ~inside:loc ~after:fields right in
      let field_indent = requirement left in
      group (left ^^ break 1) 
      ^^ nest (field_indent + 1) fields
      ^^ group (break 1 ^^ right)
  end


  module Fit_or_vertical : sig
    val pp_fields : raw -> raw list -> t

    val pp
      :  loc:Location.t
      -> left:Parser.token
      -> right:Parser.token
      -> raw
      -> raw list
      -> t
  end = struct
    let fmt (doc, attrs) =
      match attrs with
      | [] -> { doc = group doc; has_semi = false }
      | x :: xs ->
        let attrs = separate (break 0) x xs in
        {
          doc =
            prefix ~indent:2 ~spaces:1 (group (suffix ~after:doc ';')) attrs;
          has_semi = true
        }

    let rec pp_fields_aux = function
      | [] -> assert false
      | [ x ] -> (fmt x).doc
      | x :: ((_ :: _) as rest) ->
        let elt = fmt x in
        if elt.has_semi then
          elt.doc ^/^ (pp_fields_aux rest)
        else
          let rest = pp_fields_aux rest in
          let semi = Token.pp ~after:elt.doc ~before:rest SEMI in
          group (elt.doc ^^ semi) ^/^ rest

    let pp_fields x xs =
      let fields = pp_fields_aux (x :: xs) in
      nest 2 (break 1 ^^ fields)

    let pp ~loc ~left ~right x xs =
      let fields = pp_fields x xs in
      let left = Token.pp ~inside:loc ~before:fields left in
      let right = Token.pp ~inside:loc ~after:fields right in
      left ^^ fields ^/^ right
  end


  let pp ~loc ~formatting ~left ~right = function
    | [] ->
      let fst = Token.pp ~inside:loc left in
      let snd = Token.pp ~inside:loc right in
      group (fst ^/^ snd)
    | x :: xs ->
      match (formatting : Options.Wrappable.t) with
      | Wrap -> Wrapped.pp ~loc ~left ~right x xs
      | Fit_or_vertical -> Fit_or_vertical.pp ~loc ~left ~right x xs

  let pp_fields ~formatting x xs =
    match (formatting : Options.Wrappable.t) with
    | Wrap -> Wrapped.pp_fields x xs
    | Fit_or_vertical -> Fit_or_vertical.pp_fields x xs
end


module List_like = struct
  let pp ~formatting ~left ~right elts =
    let elts = List.map (fun x -> (x, [])) elts in
    Enclosed_separated.pp ~formatting ~left ~right elts

  let pp_fields ~formatting x xs =
    Enclosed_separated.pp_fields ~formatting (x, [])
      (List.map (fun x -> (x, [])) xs)
end


module Record_like = Enclosed_separated

let enclose ~loc doc left right =
  let left = Token.pp ~inside:loc ~before:doc left in
  let right = Token.pp ~inside:loc ~after:doc right in
  left ^^ doc ^^ right

let braces ~loc doc = enclose ~loc doc LBRACE RBRACE
let brackets ~loc doc = enclose ~loc doc LBRACKET RBRACKET
let parens ~loc doc = enclose ~loc doc LPAREN RPAREN

let left_assoc_map ?sep ~f first rest =
  List.fold_left (fun t elt ->
    let elt = f elt in
    match sep with
    | None -> t ^^ group (break 1 ^^ elt)
    | Some sep ->
      let sep = Token.pp ~after:t ~before:elt sep in
      t ^/^ group (sep ^/^ elt)
  ) (f first) rest

(* FIXME: merge into List_like *)
let tuple_fields pp x xs =
  List.fold_left (fun acc elt ->
    let elt = pp elt in
    let comma = Token.pp ~after:acc ~before:elt COMMA in
    acc ^^ comma ^/^ elt
  ) (pp x) xs

let case_list ~loc c cs =
  let cases =
    List.fold_left (fun acc case ->
      let bar = Token.pp ~after:acc ~before:case BAR in
      acc ^/^ bar ^^ space ^^ case
    ) c cs
  in
  match Token.pp ~expect_failure:true ~inside:loc ~before:cases BAR with
  | bar -> (ifflat empty (bar ^^ space)) ^^ cases
  | exception (Not_found | Assert_failure _) ->
    ifflat cases (PPrint.(bar ^^ space) ++ cases)
