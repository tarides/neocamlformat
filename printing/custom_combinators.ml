open PPrint

let prefix ~indent:n ~spaces:b l r = prefix n b l r
let infix ~indent:n ~spaces:b op l r = infix n b op l r

module List_like = struct
  let docked ~left ~right x xs =
    let fmt x = nest 2 (group (break 1 ^^ x)) in
    let fields =
      List.fold_left
        (fun acc elt -> group (acc ^^ semi) ^^ fmt elt)
        (fmt x) 
        xs
    in
    left ^^ fields ^^ group (break 1 ^^ right)

  let fit_or_vertical ~left ~right elts =
    left ^^ nest 2 (
      break 1 ^^ separate (semi ^^ break 1) elts
    ) ^/^ right

  let pp ~formatting ~left ~right = function
    | [] -> left ^^ right
    | x :: xs as elts ->
      match (formatting : Options.Dockable.t) with
      | Docked -> docked ~left ~right x xs
      | Fit_or_vertical -> fit_or_vertical ~left ~right elts
end

let left_assoc_map ~sep ~f = function
  | [] -> empty
  | x :: xs ->
    List.fold_left (fun doc elt -> doc ^/^ group (sep ^^ f elt)) (f x) xs
