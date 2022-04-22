open Document
open Import

module Rhs = struct
  type t =
    | Absent
    | Regular of document
    | Two_parts of document * document

  let of_opt f = function
    | None -> Absent
    | Some x -> Regular (f x)
end


type t =
  {
    lhs : document;
    params : document list;
    constr : document option;
    coerce : document option;
    rhs : Rhs.t
  }

let pp_params = function
  | [] -> empty
  | p :: ps -> nest 4 (break 1 ^^ group (flow (break 1) p ps))

let attach_annot pre doc ~sep annot =
  match annot with
  | None -> doc
  | Some annot ->
    let after = if doc == empty then pre else doc in
    let sep = Token.pp ~after ~before:annot sep in
    (prefix ~indent:2 ~spaces:1 doc sep) ^^ nest 2 (break 1 ^^ annot)

let pp
    ?(binder=Source_parsing.Parser.EQUAL) ?keyword
    { lhs; params; constr; coerce; rhs }
=
  let pre =
    match keyword with
    | None -> lhs
    | Some keyword -> prefix ~indent:2 ~spaces:1 keyword lhs
  in
  let params = pp_params params in
  let with_constraint = attach_annot pre params ~sep:COLON constr in
  let with_coercion = attach_annot pre with_constraint ~sep:COLONGREATER coerce in
  let after = if with_coercion == empty then pre else with_coercion in
  match rhs with
  | Absent -> pre ^^ with_coercion
  | Regular rhs ->
    let binder = Token.pp ~after ~before:rhs binder in
    let lhs = pre ^^ group (with_coercion ^/^ binder) in
    lhs ^^ nest 2 (break 1 ^^ rhs) (* Not prefix: no grouping *)
  | Two_parts (fst, snd) ->
    let binder = Token.pp ~after ~before:fst binder in
    let lhs = pre ^^ group (with_coercion ^/^ binder) in
    prefix ~indent:2 ~spaces:1 lhs fst ^^ nest 2 snd

let pp_simple ?binder ~keyword lhs rhs =
  pp ?binder ~keyword
    {
      lhs;
      params = [];
      constr = None;
      coerce = None;
      rhs = Regular rhs
    }

module Module = struct
  type constraint_ =
    | None
    | Sig of document
    | Mty of document
  type body =
    | Items of document
    | Generic of document
  type context =
    | Sig
    | Struct

  type t =
    {
      name : document;
      params : document list;
      constr : constraint_;
      body : body;
      attributes : document
    }

  let located_body = function
    | Generic doc | Items doc -> doc

  let pp ~keyword ~loc ~context { name; params; constr; body; attributes } =
    let pre = prefix ~indent:2 ~spaces:1 keyword name in
    let params = pp_params params in
    let after  = if params == empty then pre else params in
    let with_constraint, binder =
      let body = located_body body in
      match constr, context with
      | None, Struct -> params, Token.pp ~after ~before:body EQUAL
      | None, Sig -> params, Token.pp ~after ~before:body COLON
      | Sig sg, Struct ->
        let sep =
          let colon = Token.pp ~after ~before:sg COLON in
          let sig_ = Token.pp ~after:colon ~before:sg SIG in
          prefix ~indent:2 ~spaces:1 (group (break 1 ^^ colon)) sig_
        in
        let doc =
          group (params ^^ nest 2 sep) ^^ nest 2 (hardline ^^ sg)
        in
        let end_ = Token.pp ~after:doc ~before:body END in
        let eq = Token.pp ~after:end_ ~before:body EQUAL in
        doc, end_ ^/^ eq
      | Mty constraint_, Struct ->
        let doc = attach_annot pre params ~sep:COLON (Some constraint_) in
        doc, Token.pp ~after:doc ~before:body EQUAL
      | _, Sig -> assert false
    in
    let binder, rhs =
      match body with
      | Items items ->
        let doc =
          let end_ = Token.pp ~inside:loc ~after:items ~before:attributes END in
          nest 2 (hardline ^^ items) ^^ hardline ^^ end_
        in
        let open_ =
          Token.pp ~after:binder ~before:doc
            (match context with
            | Struct -> STRUCT
            | Sig -> SIG)
        in
        binder ^/^ open_, doc
      | Generic doc -> binder, nest 2 (break 1 ^^ doc)
    in
    let doc = pre ^^ group (with_constraint ^/^ group binder) ^^ rhs in
    group (prefix ~indent:2 ~spaces:1 doc attributes)
end

