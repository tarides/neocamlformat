open Document
open Import
open Location

type t = {
  lhs : document;
  params: document list loc;
  constr: document option;
  coerce: document option;
  rhs : document option;
}

let pp_params { loc; txt = params } =
  match params with
  | [] -> empty ~loc
  | p :: ps -> nest 4 (break_before @@ group (flow (break 1) p ps))

let attach_annot doc ~sep annot =
  match annot with
  | None -> doc
  | Some annot ->
    let sep = pp_token ~after:doc ~before:annot sep in
    (prefix ~indent:2 ~spaces:1 doc sep) ^^ nest 2 (break_before annot)

let pp ?(binder=Source_parsing.Parser.EQUAL) ?keyword
    { lhs; params; constr; coerce; rhs } =
  let pre =
    match keyword with
    | None -> lhs
    | Some keyword -> prefix ~indent:2 ~spaces:1 keyword lhs
  in
  let params = pp_params params in
  let with_constraint = attach_annot params ~sep:COLON constr in
  let with_coercion = attach_annot with_constraint ~sep:COLONGREATER coerce in
  match rhs with
  | None -> pre ^^ with_coercion
  | Some rhs ->
    let binder = pp_token ~after:with_coercion ~before:rhs binder in
    let lhs = pre ^^ group (with_coercion ^/^ binder) in
    concat ~indent:2 ~sep:(break 1) lhs rhs (* Not prefix: no grouping *)

let pp_simple ?binder ~keyword lhs rhs =
  let loc = { lhs.loc with loc_start = lhs.loc.loc_end } in
  pp ?binder ~keyword
    { lhs; params = { loc; txt = [] }; constr = None; coerce = None;
      rhs = Some rhs }

module Module = struct
  type constraint_ = None | Sig of document | Mty of document
  type body =
    | Items of document
    | Generic of document

  type context = Sig | Struct

  type t = {
    name : document;
    params: document list loc;
    constr: constraint_;
    body:  body;
    attributes: document;
  }

  let located_body = function
    | Generic doc
    | Items doc -> doc

  let pp ~keyword ~context { name; params; constr; body; attributes } =
    let pre = prefix ~indent:2 ~spaces:1 keyword name in
    let params = pp_params params in
    let with_constraint, binder =
      let body = located_body body in
      match constr, context with
      | None, Struct -> params, pp_token ~after:params ~before:body EQUAL
      | None, Sig    -> params, pp_token ~after:params ~before:body COLON
      | Sig sg, Struct ->
        let sep =
          let colon = pp_token ~after:params ~before:sg COLON in
          let sig_ = pp_token ~after:colon ~before:sg SIG in
          prefix ~indent:2 ~spaces:1 (group (break_before colon)) sig_
        in
        let doc =
          group (params ^^ nest 2 sep)
          ^^ nest 2 (PPrint.hardline ++ sg)
        in
        let end_ = pp_token ~after:doc ~before:body END in
        let eq = pp_token ~after:end_ ~before:body EQUAL in
        doc, end_ ^/^ eq
      | Mty constraint_, Struct ->
        let doc = attach_annot params ~sep:COLON (Some constraint_) in
        doc, pp_token ~after:doc ~before:body EQUAL
      | _ , Sig -> assert false
    in
    let binder, rhs =
      match body with
      | Items items ->
        let doc =
          let end_ = pp_token ~after:items ~before:attributes END in
          concat ~sep:hardline
            (nest 2 (hardline ++ items))
            end_
        in
        let open_ =
          pp_token ~after:binder ~before:doc
            (match context with Struct -> STRUCT | Sig -> SIG)
        in
        binder ^/^ open_, doc
      | Generic doc ->
        binder, nest 2 (break_before doc)
    in
    let doc = pre ^^ group (with_constraint ^/^ group binder) ^^ rhs in
    group (prefix ~indent:2 ~spaces:1 doc attributes)
end
