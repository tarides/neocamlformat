open Source_parsing
open Source_tree

module Attribute = struct
  let has_non_doc =
    List.exists (fun attr ->
      match attr.attr_name.txt with
      | "ocaml.doc" | "ocaml.text" -> false
      | _ -> true
    )
end

let rec parens_expression ps e =
  let obj = object(self)
    inherit [Printing_stack.t] Fold_map.fold_map as super

    method! expression ps e =
      let ps, enclose =
        if Attribute.has_non_doc e.pexp_attributes
        then Printing_stack.parenthesize_exp (Attribute :: ps)
        else ps, false
      in
      let ps = Printing_stack.Expression e.pexp_desc :: ps in
      match e.pexp_desc with
      | Pexp_let _ -> self#pexp_let enclose ps e
      | desc ->
        let ps, pexp_desc = self#expression_desc ps desc in
        let e = { e with pexp_desc } in
        if enclose
        then Ast_helper.Exp.parens ~loc:e.pexp_loc e
        else e

    method! expression_desc ps desc =
      let ps =
        match desc with
        | Pexp_parens _ -> []
        | _ -> ps
      in
      super#expression_desc ps desc

    method pexp_let enclose ps e =
      match e.pexp_desc with
      | Pexp_let (rf, vbs, body) ->
        let vbs = List.map (self#value_binding []) vbs in
        let ps, enclose' = Printing_stack.parenthesize_exp ps in
        let enclose = enclose || enclose' in
        let body = pp ps body in
        let e = { e with pexp_desc = Pexp_let (rf, vbs, body) } in
        if enclose
        then Ast_helper.Exp.parens ~loc:e.pexp_loc e
        else e
      | _ -> assert false
  end in
  obj#expression ps e

and parens_desc _ps desc =
  match desc with
  | Pexp_parens t -> Pexp_parens { t with exp = parens_expression [] t.exp }
  | Pexp_ident _ 
  | Pexp_constant _ -> desc
  | _ -> failwith "TODO"
    (*
  | Pexp_let (rf, vbs, body) -> pp_let ps rf vbs body
    | Pexp_function cases -> pp_function ~loc ~ext_attrs ps cases
    | Pexp_fun (params, exp) -> pp_fun ~loc ~ext_attrs ps params exp
    | Pexp_apply (expr, args) -> Application.pp ps expr args
    | Pexp_match (arg, cases) -> pp_match ~loc ~ext_attrs ps arg cases
    | Pexp_try (arg, cases) -> pp_try ~loc ~ext_attrs ps arg cases
    | Pexp_tuple exps -> pp_tuple ps exps
    | Pexp_list_lit exps -> pp_list_literal ~loc ps exps
    | Pexp_cons (hd, tl) -> pp_cons ps hd tl
    | Pexp_construct (lid, arg) -> pp_construct ps lid arg
    | Pexp_variant (tag, arg) -> pp_variant ps tag arg
    | Pexp_record (fields, exp) -> pp_record ~loc ps fields exp
    | Pexp_field (exp, fld) -> pp_field ps exp fld
    | Pexp_setfield (exp, fld, val_) -> pp_setfield ps exp fld val_
    | Pexp_array elts -> pp_array ~loc ps elts
    | Pexp_ifthen branches -> pp_if_then ps branches
    | Pexp_ifthenelse (branches, else_) -> pp_if_then_else ps branches else_
    | Pexp_sequence (e1, e2) -> pp_sequence ps e1 e2
    | Pexp_while (cond, body) -> pp_while ~loc ~ext_attrs ps cond body
    | Pexp_for (it, start, stop, dir, body) ->
      pp_for ~loc ~ext_attrs ps it start stop dir body
    | Pexp_constraint (e, ct) -> pp_constraint e ct
    | Pexp_coerce (e, ct_start, ct) -> pp_coerce e ct_start ct
    | Pexp_send (e, meth) -> pp_send ps e meth
    | Pexp_new lid -> pp_new ~loc ~ext_attrs ps lid
    | Pexp_setinstvar (lbl, exp) -> pp_setinstvar ps lbl exp
    | Pexp_override fields -> pp_override ~loc fields
    | Pexp_letmodule (name, mb, body) ->
      pp_letmodule ~loc ~ext_attrs ps name mb body
    | Pexp_letexception (exn, exp) -> pp_letexception ~loc ~ext_attrs ps exn exp
    | Pexp_assert exp -> pp_assert ~loc ~ext_attrs ps exp
    | Pexp_lazy exp -> pp_lazy ~loc ~ext_attrs ps exp
    | Pexp_object cl -> pp_object ~loc ~ext_attrs ps cl
    | Pexp_pack (me, pkg) -> pp_pack ~loc ~ext_attrs me pkg
    | Pexp_open (lid, exp) -> pp_open lid exp
    | Pexp_letopen (od, exp) -> pp_letopen ~loc ~ext_attrs ps od exp
    | Pexp_letop letop -> pp_letop ps letop
    | Pexp_extension ext -> Extension.pp Item ext
    | Pexp_unreachable -> string ~loc "."
    | Pexp_array_get (arr, idx) -> pp_array_get ps arr idx
    | Pexp_array_set (arr, idx, e) -> pp_array_set ps arr idx e
    | Pexp_string_get (str, idx) -> pp_string_get ps str idx
    | Pexp_string_set (str, idx, c) -> pp_string_set ps str idx c
    | Pexp_bigarray_get (ba, idx) -> pp_bigarray_get ps ba idx
    | Pexp_bigarray_set (ba, idx, c) -> pp_bigarray_set ps ba idx c
    | Pexp_dotop_get { accessed; op; left; right; indices } ->
      pp_dotop_get ps accessed op left right indices
    | Pexp_dotop_set { accessed; op; left; right; indices; value } ->
      pp_dotop_set ps accessed op left right indices value

       *)
