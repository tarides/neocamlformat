open PPrint
open Custom_combinators

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

module Module = struct
  type constraint_ = None | Sig of document | Mty of document
  type expr = Struct of document | Expr of document

  type t = {
    name : document;
    params: document list;
    constr: constraint_;
    expr:  expr;
    attributes: document;
  }

  let pp ~keyword { name; params; constr; expr; attributes } =
    let pre = group (keyword ^^ nest 2 (break 1 ^^ name)) in
    let params =
      match params with
      | [] -> empty
      | params -> nest 4 (break 1 ^^ group (flow (break 1) params))
    in
    let with_constraint, binder =
      match constr with
      | None -> params, equals
      | Sig sg ->
        let doc =
          group (
            params ^^ nest 2 (
              group (
                group (break 1 ^^ colon) ^^ nest 2 (break 1 ^^ !^"sig")
              )
            )
          )
          ^^ nest 2 (hardline ^^ sg)
        in
        doc, !^"end ="
      | Mty constraint_ ->
        let doc =
          group (params ^^ nest 2 (break 1 ^^ colon))
          ^^ nest 2 (break 1 ^^ constraint_)
        in
        doc, equals
    in
    let binder, rhs =
      match expr with
      | Struct str ->
        let doc = nest 2 (hardline ^^ str) ^^ hardline ^^ !^"end" in
        binder ^^ !^" struct", doc
      | Expr doc ->
        binder, nest 2 (break 1 ^^ doc)
    in
    let doc = pre ^^ group (with_constraint ^/^ binder) ^^ rhs in
    group (prefix ~indent:2 ~spaces:1 doc attributes)
end