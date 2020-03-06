open PPrint

type t = {
  lhs : document;
  params: document list;
  constr: document option;
  coerce: document option;
  rhs : document;
}

let pp ?(binder=equals) ~keyword { lhs; params; constr; coerce; rhs } =
  let pre = group (keyword ^^ nest 2 (break 1 ^^ lhs)) in
  let params =
    match params with
    | [] -> empty
    | params -> nest 4 (break 1 ^^ group (flow (break 1) params))
  in
  let with_constraint =
    match constr with
    | None -> params
    | Some constraint_ ->
      group (params ^^ nest 2 (break 1 ^^ colon))
      ^^ nest 2 (break 1 ^^ constraint_)
  in
  let with_coercion =
    match coerce with
    | None -> with_constraint
    | Some coercion ->
      group (with_constraint ^^ nest 2 (break 1 ^^ !^":>"))
      ^^ nest 2 (break 1 ^^ coercion)
  in
  let lhs = pre ^^ group (with_coercion ^/^ binder) in
  lhs ^^ nest 2 (break 1 ^^ rhs)

let pp_simple ?binder ~keyword lhs rhs =
  pp ?binder ~keyword { lhs; params = []; constr = None; coerce = None; rhs}
