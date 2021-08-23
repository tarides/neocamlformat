open Source_parsing.Location
open Document
type document = t

let pp_item_attr : (Source_parsing.Source_tree.attribute -> document) ref =
  ref (fun _ -> failwith "not instantiated yet")

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
    group (doc ^^ nest 2 (break_before sep)) ^^ nest 2 (break_before annot)

type binding_kw = {
  token: document;
  extension: string loc option;
  attrs: Source_parsing.Source_tree.attributes;
  modifier: Source_parsing.Parser.token option;
}

let pp_kw lhs { token; extension; attrs; modifier } =
  let kw =
    match extension with
    | None -> token
    | Some ext ->
      let percent = token_between token lhs PERCENT in
      token ^^ percent ^^ str ext
  in
  let kw =
    match attrs with
    | [] -> kw
    | attr :: attrs ->
      group (
        prefix ~indent:2 ~spaces:0 kw
          (separate_map (PPrint.break 0) ~f:!pp_item_attr attr attrs)
      )
  in
  match modifier with
  | None -> kw
  | Some tok ->
    let modif = token_between kw lhs tok in
    kw ^/^ modif

let pp ?(binder=Source_parsing.Parser.EQUAL) ?keyword
    { lhs; params; constr; coerce; rhs } =
  let keyword = Option.map (pp_kw lhs) keyword in
  let pre =
    match keyword with
    | None -> lhs
    | Some keyword -> group (keyword ^^ nest 2 (break_before lhs))
  in
  let params = pp_params params in
  let with_constraint = attach_annot params ~sep:COLON constr in
  let with_coercion = attach_annot with_constraint ~sep:COLONGREATER coerce in
  match rhs with
  | None -> pre ^^ with_coercion
  | Some rhs ->
    let binder = token_between with_coercion rhs binder in
    let lhs = pre ^^ group (with_coercion ^/^ binder) in
    group (lhs ^^ nest 2 (break_before rhs))

let pp_simple ?binder ~keyword lhs rhs =
  let loc = { lhs.loc with loc_start = lhs.loc.loc_end } in
  pp ?binder
    ~keyword:{ token = keyword; extension = None; attrs = []; modifier = None }
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
    let pre = group (keyword ^^ nest 2 (break_before name)) in
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
