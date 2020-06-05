open Parsetree
open Ast_helper

let sort_attributes : attributes -> attributes = List.sort compare

let normalize_cmt_spaces doc =
  String.split_on_char ' ' doc
  |> List.filter ((<>) "")
  |> String.concat " "

let ignore_docstrings = ref false

let mapper =
  (* remove locations *)
  let location _ _ = Location.none in
  let attribute (m : Ast_mapper.mapper) (attr : attribute) =
    let attr =
      match attr.attr_name.txt with
      | "ocaml.doc" | "ocaml.txt" ->
        let attr_payload =
          match attr.attr_payload with
          | PStr [ {
              pstr_desc =
                Pstr_eval
                  ({ pexp_desc= Pexp_constant (Pconst_string (doc, None)) ; _ }
                   as inner_exp
                  ,[]);
              _
            } as str ] ->
            let doc = normalize_cmt_spaces doc in
            let inner' =
              { inner_exp with
                pexp_desc = Pexp_constant (Pconst_string (doc, None)) }
            in
            PStr [ { str with pstr_desc = Pstr_eval (inner', []) }]
          | _ -> assert false
        in
        { attr with attr_payload }
      | _ -> attr
    in
    Ast_mapper.default_mapper.attribute m attr
  in
  let attributes (m : Ast_mapper.mapper) (attrs : attribute list) =
    let attrs =
      if not !ignore_docstrings then
        attrs
      else
        List.filter (function
          | { attr_name = { txt = ("ocaml.doc" | "ocaml.txt"); _ }; _ } ->
            false
          | _ -> true
        ) attrs
    in
    (* sort attributes *)
    Ast_mapper.default_mapper.attributes m (sort_attributes attrs)
  in
  let expr (m : Ast_mapper.mapper) exp =
    let exp = {exp with pexp_loc_stack= []} in
    let {pexp_desc; pexp_attributes; _} = exp in
    match pexp_desc with
    (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
    | Pexp_sequence
        ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []; _}, e3) ->
        m.expr m
          (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3))
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  let pat (m : Ast_mapper.mapper) pat =
    let pat = {pat with ppat_loc_stack= []} in
    let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1; _} = pat in
    (* normalize nested or patterns *)
    match ppat_desc with
    | Ppat_or
        ( pat1
        , { ppat_desc= Ppat_or (pat2, pat3)
          ; ppat_loc= loc2
          ; ppat_attributes= attrs2
          ; _ } ) ->
        m.pat m
          (Pat.or_ ~loc:loc1 ~attrs:attrs1
             (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
             pat3)
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let typ (m : Ast_mapper.mapper) typ =
    let typ = {typ with ptyp_loc_stack= []} in
    Ast_mapper.default_mapper.typ m typ
  in
  (*
  let value_binding (m : Ast_mapper.mapper) vb =
    let { pvb_pat= {ppat_desc; ppat_loc; ppat_attributes; _}
        ; pvb_expr
        ; pvb_loc
        ; pvb_attributes } =
      vb
    in
    match (ppat_desc, pvb_expr.pexp_desc) with
    (* recognize and undo the pattern of code introduced by
       ocaml/ocaml@fd0dc6a0fbf73323c37a73ea7e8ffc150059d6ff to fix
       https://caml.inria.fr/mantis/view.php?id=7344 *)
    | ( Ppat_constraint
          ( ({ppat_desc= Ppat_var _; _} as p0)
          , {ptyp_desc= Ptyp_poly ([], t0); _} )
      , Pexp_constraint (e0, t1) )
      when equal_core_type t0 t1 ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes e0 t0))
    (* convert [let (x : t) = e] to [let x = (e : t)] *)
    | Ppat_constraint (p0, t0), _ ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes pvb_expr
                t0))
    | _ -> Ast_mapper.default_mapper.value_binding m vb
  in
     *)
  let structure_item (m : Ast_mapper.mapper) (si : structure_item) =
    match si.pstr_desc with
    | Pstr_eval ({pexp_desc= Pexp_extension e; _}, []) ->
        let e = m.extension m e in
        let pstr_loc = m.location m si.pstr_loc in
        {pstr_desc= Pstr_extension (e, []); pstr_loc}
    | _ -> Ast_mapper.default_mapper.structure_item m si
  in
  { Ast_mapper.default_mapper with
    location
  ; attribute
  ; attributes
  ; expr
  ; pat
  ; typ
  ; structure_item }

let check_same_ast ~impl s1 s2 =
  let lex1 = Lexing.from_string s1 in
  let lex2 = Lexing.from_string s2 in
  if impl then
    let ast1 =
      Parse.implementation lex1
      |> mapper.structure mapper
    in
    let ast2 =
      Parse.implementation lex2
      |> mapper.structure mapper
    in
    ast1 = ast2
  else
    let ast1 =
      Parse.interface lex1
      |> mapper.signature mapper
    in
    let ast2 =
      Parse.interface lex2
      |> mapper.signature mapper
    in
    ast1 = ast2
