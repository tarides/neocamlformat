open Source_parsing.Location
open Document
type document = t

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
    let sep = token_between doc annot sep in
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
    let binder = token_between with_coercion rhs binder in
    let lhs = pre ^^ group (with_coercion ^/^ binder) in
    prefix ~indent:2 ~spaces:1 lhs rhs

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

  let pp ~keyword ~context { name; params; constr; body; attributes } =
    let pre = prefix ~indent:2 ~spaces:1 keyword name in
    let params = pp_params params in
    let with_constraint, binder =
      match constr, context with
      | None, Struct -> params, "="
      | None, Sig -> params, ":"
      | Sig sg, Struct ->
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
      | Mty constraint_, Struct ->
        let doc = attach_annot params ~sep:COLON (Some constraint_) in
        doc, "="
      | _ , Sig -> assert false
    in
    let binder, rhs =
      match body with
      | Items items ->
        let doc =
          enclose ~before:PPrint.empty ~after:PPrint.(hardline ^^ !^"end")
            (nest 2 (hardline ++ items))
        in
        binder ^ (match context with Struct -> " struct" | Sig -> " sig"), doc
      | Generic doc ->
        binder, nest 2 (break_before doc)
    in
    let binder = (* Gloups. *)
      let fake =
        token_between with_constraint rhs
          (match context with
           | Struct -> EQUAL
           | Sig -> COLON)
      in
      string ~loc:fake.loc binder
    in
    let doc = pre ^^ group (with_constraint ^/^ binder) ^^ rhs in
    group (prefix ~indent:2 ~spaces:1 doc attributes)
end
