open Source_parsing.Location
open Document
type document = t

type t = {
  lhs : document;
  params: document list loc;
  constr: document option;
  coerce: document option;
  rhs : document;
}

let pp_params { loc; txt = params } =
  match params with
  | [] -> empty ~loc
  | p :: ps -> nest 4 (break_before @@ group (flow (break 1) p ps))

let attach_annot doc ~sep annot =
  match annot with
  | None -> doc
  | Some annot ->
    let sep = token_between doc annot sep in
    group (doc ^^ nest 2 (break_before sep)) ^^ nest 2 (break_before annot)

let pp ?(binder=Equals) ~keyword { lhs; params; constr; coerce; rhs } =
  let pre = group (keyword ^^ nest 2 (break_before lhs)) in
  let params = pp_params params in
  let with_constraint = attach_annot params ~sep:Colon constr in
  let with_coercion = attach_annot with_constraint ~sep:Coerce coerce in
  let binder = token_between with_coercion rhs binder in
  let lhs = pre ^^ group (with_coercion ^/^ binder) in
  group (lhs ^^ nest 2 (break_before rhs))

let pp_simple ?binder ~keyword lhs rhs =
  let loc = { lhs.loc with loc_start = lhs.loc.loc_end } in
  pp ?binder ~keyword
    { lhs; params = { loc; txt = [] }; constr = None; coerce = None; rhs}

module Module = struct
  type constraint_ = None | Sig of document | Mty of document
  type expr = Struct of document | Expr of document

  type t = {
    name : document;
    params: document list loc;
    constr: constraint_;
    expr:  expr;
    attributes: document;
  }

  let pp ~keyword { name; params; constr; expr; attributes } =
    let pre = group (keyword ^^ nest 2 (break_before name)) in
    let params = pp_params params in
    let with_constraint, binder =
      match constr with
      | None -> params, "="
      | Sig sg ->
        let sep =
          let txt =
            let open PPrint in
            group (group (break 1 ^^ colon) ^^ nest 2 (break 1 ^^ !^"sig"))
          in
          { txt; loc = loc_between params sg }
        in
        let doc =
          group (params ^^ nest 2 sep)
          ^^ nest 2 (PPrint.hardline ++ sg)
        in
        doc, "end ="
      | Mty constraint_ ->
        let doc = attach_annot params ~sep:Colon (Some constraint_) in
        doc, "="
    in
    let binder, rhs =
      match expr with
      | Struct str ->
        let doc =
          enclose ~before:PPrint.empty ~after:PPrint.(hardline ^^ !^"end")
            (nest 2 (hardline ++ str))
        in
        binder ^ " struct", doc
      | Expr doc ->
        binder, nest 2 (break_before doc)
    in
    let binder = (* Gloups. *)
      let fake = token_between with_constraint rhs Equals in
      string ~loc:fake.loc binder
    in
    let doc = pre ^^ group (with_constraint ^/^ binder) ^^ rhs in
    group (prefix ~indent:2 ~spaces:1 doc attributes)
end
