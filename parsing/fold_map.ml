open Source_tree

let option f acc = function
  | None -> acc, None
  | Some x ->
    let acc, x = f acc x in
    acc, Some x

let rec list f acc = function
  | [] -> acc, [] 
  | x :: xs ->
    let acc, x = f acc x in
    let acc, xs = list f acc xs in
    acc, x :: xs

let loc f acc { Location.txt; loc } =
  let acc, txt = f acc txt in
  acc, { Location.txt; loc }

class ['acc] fold_map = object(self)
  method attribute (acc : 'acc) attr =
    let acc, attr_payload = self#payload acc attr.attr_payload in
    acc, { attr with attr_payload }

  method extension acc (s, payload) =
    let acc, payload = self#payload acc payload in
    let ext = (s, payload) in
    acc, ext

  method attributes = list self#attribute

  method payload acc = function
    | PStr str ->
      let acc, str = self#structure acc str in
      acc, PStr str
    | PSig sg ->
      let acc, sg = self#signature acc sg in
      acc, PSig sg
    | PTyp ct ->
      let acc, ct = self#core_type acc ct in
      acc, PTyp ct
    | PPat (pat, exp) ->
      let acc, pat = self#pattern acc pat in
      let acc, exp = option self#expression acc exp in
      acc, PPat (pat, exp)

  method core_type acc ct =
    let acc, ptyp_desc = self#core_type_desc acc ct.ptyp_desc in
    let acc, ptyp_attributes = self#attributes acc ct.ptyp_attributes in
    acc, { ct with ptyp_desc; ptyp_attributes }

  method core_type_desc acc d =
    match d with
    | Ptyp_any
    | Ptyp_var _ -> acc, d
    | Ptyp_arrow (lst, ct) ->
      let acc, lst =
        list (fun acc (l, ct) ->
          let acc, ct = self#core_type acc ct in
          acc, (l, ct)
        ) acc lst
      in
      let acc, ct = self#core_type acc ct in
      acc, Ptyp_arrow (lst, ct)
    | Ptyp_tuple cts ->
      let acc, cts = list self#core_type acc cts in
      acc, Ptyp_tuple cts
    | Ptyp_constr (lid, cts) ->
      let acc, cts = list self#core_type acc cts in
      acc, Ptyp_constr (lid, cts)
    | Ptyp_object (ofs, cf) ->
      let acc, ofs = list self#object_field acc ofs in
      acc, Ptyp_object (ofs, cf)
    | Ptyp_class (lid, cts) ->
      let acc, cts = list self#core_type acc cts in
      acc, Ptyp_class (lid, cts)
    | Ptyp_alias (ct, var) ->
      let acc, ct = self#core_type acc ct in
      acc, Ptyp_alias (ct, var)
    | Ptyp_variant (rfs, cf, lbls) -> 
      let acc, rfs = list self#row_field acc rfs in
      acc, Ptyp_variant (rfs, cf, lbls)
    | Ptyp_poly (vars, ct) ->
      let acc, ct = self#core_type acc ct in
      acc, Ptyp_poly (vars, ct)
    | Ptype_poly (vars, ct) ->
      let acc, ct = self#core_type acc ct in
      acc, Ptype_poly (vars, ct)
    | Ptyp_package pkg ->
      let acc, pkg = self#package_type acc pkg in
      acc, Ptyp_package pkg
    | Ptyp_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Ptyp_extension ext

  method package_type acc (lid, cstrs) =
    let acc, cstrs =
      list (fun acc (lid, ct) ->
        let acc, ct = self#core_type acc ct in
        acc, (lid, ct)
        ) acc cstrs
    in
    acc, (lid, cstrs)

  method row_field acc { prf_desc ; prf_loc ; prf_attributes } =
    let acc, prf_desc = self#row_field_desc acc prf_desc in
    let acc, prf_attributes = self#attributes acc prf_attributes in
    acc, { prf_desc; prf_loc; prf_attributes }

  method row_field_desc acc = function
    | Rtag (lbl, b, cts) ->
      let acc, cts = list self#core_type acc cts in
      acc, Rtag (lbl, b, cts)
    | Rinherit ct ->
      let acc, ct = self#core_type acc ct in
      acc, Rinherit ct

  method object_field acc { pof_desc ; pof_loc ; pof_attributes } =
    let acc, pof_desc = self#object_field_desc acc pof_desc in
    let acc, pof_attributes = self#attributes acc pof_attributes in
    acc, { pof_desc; pof_loc; pof_attributes }

  method object_field_desc acc = function
    | Otag (lbl, ct) ->
      let acc, ct = self#core_type acc ct in
      acc, Otag (lbl, ct)
    | Oinherit ct ->
      let acc, ct = self#core_type acc ct in
      acc, Oinherit ct

  method pattern acc pat =
    let acc, ppat_desc = self#pattern_desc acc pat.ppat_desc in
    let acc, ppat_attributes = self#attributes acc pat.ppat_attributes in
    acc, { pat with ppat_desc; ppat_attributes }

  method pattern_desc acc = function
    | Ppat_any
    | Ppat_var _ 
    | Ppat_constant _
    | Ppat_interval _
    | Ppat_type _
      as desc -> acc, desc
    | Ppat_alias (p, alias) ->
      let acc, p = self#pattern acc p in
      acc, Ppat_alias (p, alias)
    | Ppat_tuple ps ->
      let acc, ps = list self#pattern acc ps in
      acc, Ppat_tuple ps
    | Ppat_construct (lid, po) ->
      let acc, po = option self#pattern acc po in
      acc, Ppat_construct (lid, po)
    | Ppat_list_lit ps ->
      let acc, ps = list self#pattern acc ps in
      acc, Ppat_list_lit ps
    | Ppat_cons (p1, p2) ->
      let acc, p1 = self#pattern acc p1 in
      let acc, p2 = self#pattern acc p2 in
      acc, Ppat_cons (p1, p2)
    | Ppat_variant (lbl, po) ->
      let acc, po = option self#pattern acc po in
      acc, Ppat_variant (lbl, po)
    | Ppat_record (lbls, cf) ->
      let acc, lbls =
        list (fun acc (lid, cto, po) ->
          let acc, cto = option self#core_type acc cto in
          let acc, po = option self#pattern acc po in
          acc, (lid, cto, po)
        ) acc lbls
      in
      acc, Ppat_record (lbls, cf)
    | Ppat_array ps ->
      let acc, ps = list self#pattern acc ps in
      acc, Ppat_array ps
    | Ppat_or (p1, p2) ->
      let acc, p1 = self#pattern acc p1 in
      let acc, p2 = self#pattern acc p2 in
      acc, Ppat_or (p1, p2)
    | Ppat_constraint (p, ct) ->
      let acc, p = self#pattern acc p in
      let acc, ct = self#core_type acc ct in
      acc, Ppat_constraint (p, ct)
    | Ppat_lazy p ->
      let acc, p = self#pattern acc p in
      acc, Ppat_lazy p
    | Ppat_unpack (lid, pkg) ->
      let acc, pkg = option self#package_type acc pkg in
      acc, Ppat_unpack (lid, pkg)
    | Ppat_exception p ->
      let acc, p = self#pattern acc p in
      acc, Ppat_exception p
    | Ppat_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Ppat_extension ext
    | Ppat_open (lid, p) ->
      let acc, p = self#pattern acc p in
      acc, Ppat_open (lid, p)

(* Value expressions *)

  method expression acc exp =
    let acc, pexp_desc = self#expression_desc acc exp.pexp_desc in
    let acc, pexp_attributes = self#attributes acc exp.pexp_attributes in
    let acc, pexp_ext_attributes =
      let acc, attrs = self#attributes acc (snd exp.pexp_ext_attributes) in
      acc, (fst exp.pexp_ext_attributes, attrs)
    in
    acc, { exp with pexp_desc; pexp_attributes; pexp_ext_attributes }

  method fun_param acc = function
    | Term (lbl, eo, (po, cto)) ->
      let acc, eo = option self#expression acc eo in
      let acc, po = option self#pattern acc po in
      let acc, cto= option self#core_type acc cto in
      acc, Term (lbl, eo, (po, cto))
    | Type _ as x -> acc, x

  method expression_desc acc = function
    | Pexp_parens { begin_end; exp } ->
      let acc, exp = self#expression acc exp in
      acc, Pexp_parens { begin_end; exp }
    | Pexp_ident _
    | Pexp_constant _
    | Pexp_new _
    | Pexp_unreachable
        as desc -> acc, desc
    | Pexp_let (rf, vbs, e) ->
      let acc, vbs = list self#value_binding acc vbs in
      let acc, e = self#expression acc e in
      acc, Pexp_let (rf, vbs, e)
    | Pexp_function cases ->
      let acc, cases = list self#case acc cases in
      acc, Pexp_function cases
    | Pexp_fun (ps, e) ->
      let acc, ps = list self#fun_param acc ps in
      let acc, e = self#expression acc e in
      acc, Pexp_fun (ps, e)
    | Pexp_apply (e, args) ->
      let acc, e = self#expression acc e in
      let acc, args =
        list (fun acc (lbl, e) ->
          let acc, e = self#expression acc e in
          acc, (lbl, e)
        ) acc args
      in
      acc, Pexp_apply (e, args)
    | Pexp_match (e, cases) ->
      let acc, e = self#expression acc e in
      let acc, cases = list self#case acc cases in
      acc, Pexp_match (e, cases)
    | Pexp_try (e, cases) ->
      let acc, e = self#expression acc e in
      let acc, cases = list self#case acc cases in
      acc, Pexp_try (e, cases)
    | Pexp_tuple es ->
      let acc, es = list self#expression acc es in
      acc, Pexp_tuple es
    | Pexp_construct (lid, eo) ->
      let acc, eo = option self#expression acc eo in
      acc, Pexp_construct (lid, eo)
    | Pexp_list_lit es ->
      let acc, es = list self#expression acc es in
      acc, Pexp_list_lit es
    | Pexp_cons (e1, e2) ->
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      acc, Pexp_cons (e1, e2)
    | Pexp_variant (lbl, eo) ->
      let acc, eo = option self#expression acc eo in
      acc, Pexp_variant (lbl, eo)
    | Pexp_record (fields, eo) ->
      let acc, fields =
        list (fun acc (lid, (co1, co2), eo) ->
          let acc, co1 = option self#core_type acc co1 in
          let acc, co2 = option self#core_type acc co2 in
          let acc, eo = option self#expression acc eo in
          acc, (lid, (co1, co2), eo)
        ) acc fields
      in
      let acc, eo = option self#expression acc eo in
      acc, Pexp_record (fields, eo)
    | Pexp_field (e, lid) ->
      let acc, e = self#expression acc e in
      acc, Pexp_field (e, lid)
    | Pexp_setfield (e1, lid, e2) ->
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      acc, Pexp_setfield (e1, lid, e2)
    | Pexp_array es ->
      let acc, es = list self#expression acc es in
      acc, Pexp_array es
    | Pexp_ifthen ibs ->
      let acc, ibs = list self#if_branch acc ibs in
      acc, Pexp_ifthen ibs
    | Pexp_ifthenelse (ibs, e) ->
      let acc, ibs = list self#if_branch acc ibs in
      let acc, e = self#expression acc e in
      acc, Pexp_ifthenelse (ibs, e)
    | Pexp_sequence (e1, e2) ->
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      acc, Pexp_sequence (e1, e2)
    | Pexp_while (e1, e2) ->
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      acc, Pexp_while (e1, e2)
    | Pexp_for (p, e1, e2, dir, e3) ->
      let acc, p = self#pattern acc p in
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      let acc, e3 = self#expression acc e3 in
      acc, Pexp_for (p, e1, e2, dir, e3)
    | Pexp_constraint (e, ct) ->
      let acc, e = self#expression acc e in
      let acc, ct = self#core_type acc ct in
      acc, Pexp_constraint (e, ct)
    | Pexp_coerce (e, cto, ct) ->
      let acc, e = self#expression acc e in
      let acc, cto = option self#core_type acc cto in
      let acc, ct = self#core_type acc ct in
      acc, Pexp_coerce (e, cto, ct)
    | Pexp_send (e, lbl) ->
      let acc, e = self#expression acc e in
      acc, Pexp_send (e, lbl)
    | Pexp_setinstvar (lbl, e) ->
      let acc, e = self#expression acc e in
      acc, Pexp_setinstvar (lbl, e)
    | Pexp_override fields ->
      let acc, fields =
        list (fun acc (lbl, e) ->
          let acc, e = self#expression acc e in
          acc, (lbl, e)
        ) acc fields
      in
      acc, Pexp_override fields
    | Pexp_letmodule (name, (fps, mtyo, me), e) ->
      let acc, fps = list (loc self#functor_parameter) acc fps in
      let acc, mtyo = option self#module_type acc mtyo in
      let acc, me = self#module_expr acc me in
      let acc, e = self#expression acc e in
      acc, Pexp_letmodule (name, (fps, mtyo, me), e)
    | Pexp_letexception (ec, e) ->
      let acc, ec = self#extension_constructor acc ec in
      let acc, e = self#expression acc e in
      acc, Pexp_letexception (ec, e)
    | Pexp_assert e ->
      let acc, e = self#expression acc e in
      acc, Pexp_assert e
    | Pexp_lazy e ->
      let acc, e = self#expression acc e in
      acc, Pexp_lazy e
    | Pexp_object c ->
      let acc, c = self#class_structure acc c in
      acc, Pexp_object c
    | Pexp_pack (me, pkgo) ->
      let acc, me = self#module_expr acc me in
      let acc, pkgo = option self#package_type acc pkgo in
      acc, Pexp_pack (me, pkgo)
    | Pexp_open (lid, e) ->
      let acc, e = self#expression acc e in
      acc, Pexp_open (lid, e)
    | Pexp_letopen (od, e) ->
      let acc, od = self#open_declaration acc od in
      let acc, e = self#expression acc e in
      acc, Pexp_letopen (od, e)
    | Pexp_letop op ->
      let acc, op = self#letop acc op in
      acc, Pexp_letop op
    | Pexp_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Pexp_extension ext
    | Pexp_array_get (e1, e2) ->
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      acc, Pexp_array_get (e1, e2)
    | Pexp_array_set (e1, e2, e3) ->
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      let acc, e3 = self#expression acc e3 in
      acc, Pexp_array_set (e1, e2, e3)
    | Pexp_string_get (e1, e2) ->
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      acc, Pexp_string_get (e1, e2)
    | Pexp_string_set (e1, e2, e3) ->
      let acc, e1 = self#expression acc e1 in
      let acc, e2 = self#expression acc e2 in
      let acc, e3 = self#expression acc e3 in
      acc, Pexp_string_set (e1, e2, e3)
    | Pexp_bigarray_get (e1, es) ->
      let acc, e1 = self#expression acc e1 in
      let acc, es = list self#expression acc es in
      acc, Pexp_bigarray_get (e1, es)
    | Pexp_bigarray_set (e1, es, e3) ->
      let acc, e1 = self#expression acc e1 in
      let acc, es = list self#expression acc es in
      let acc, e3 = self#expression acc e3 in
      acc, Pexp_bigarray_set (e1, es, e3)
    | Pexp_dotop_get r ->
      let acc, accessed = self#expression acc r.accessed in
      let acc, indices = list self#expression acc r.indices in
      acc, Pexp_dotop_get { r with accessed; indices }
    | Pexp_dotop_set r ->
      let acc, accessed = self#expression acc r.accessed in
      let acc, indices = list self#expression acc r.indices in
      let acc, value = self#expression acc r.value in
      acc, Pexp_dotop_set { r with accessed; indices; value }

  method if_branch acc ifb =
    let acc, if_cond = self#expression acc ifb.if_cond in
    let acc, if_body = self#expression acc ifb.if_body in
    let acc, if_attrs = self#attributes acc ifb.if_attrs in
    acc, { ifb with if_cond; if_body; if_attrs }

  method case acc { pc_lhs; pc_guard; pc_rhs } =
    let acc, pc_lhs = self#pattern acc pc_lhs in
    let acc, pc_guard = option self#expression acc pc_guard in
    let acc, pc_rhs = self#expression acc pc_rhs in
    acc, { pc_lhs; pc_guard; pc_rhs }

  method letop acc { let_; ands; body } =
    let acc, let_ = self#binding_op acc let_ in
    let acc, ands = list self#binding_op acc ands in
    let acc, body = self#expression acc body in
    acc, { let_; ands; body }

  method binding_op acc bo =
    let acc, pbop_pat = self#pattern acc bo.pbop_pat in
    let acc, pbop_params = list self#fun_param acc bo.pbop_params in
    let acc, pbop_type =
      let cto1, cto2 = bo.pbop_type in
      let acc, cto1 = option self#core_type acc cto1 in
      let acc, cto2 = option self#core_type acc cto2 in
      acc, (cto1, cto2)
    in
    let acc, pbop_exp = self#expression acc bo.pbop_exp in
    acc, { bo with pbop_pat; pbop_params; pbop_type; pbop_exp }

  method value_description acc vd =
    let acc, pval_type = self#core_type acc vd.pval_type in
    let acc, pval_attributes = self#attributes acc vd.pval_attributes in
    acc, { vd with pval_type; pval_attributes }

  method type_declaration acc td =
    let acc, ptype_params =
      list (fun acc (ct, v) ->
        let acc, ct = self#core_type acc ct in
        acc, (ct, v)
      ) acc td.ptype_params
    in
    let acc, ptype_cstrs =
      list (fun acc (ct1, ct2, loc) ->
        let acc, ct1 = self#core_type acc ct1 in
        let acc, ct2 = self#core_type acc ct2 in
        acc, (ct1, ct2, loc)
      ) acc td.ptype_cstrs
    in
    let acc, ptype_kind = self#type_kind acc td.ptype_kind in
    let acc, ptype_manifest = option self#core_type acc td.ptype_manifest in
    let acc, ptype_attributes = self#attributes acc td.ptype_attributes in
    acc, { td with ptype_params; ptype_cstrs; ptype_kind; ptype_manifest;
                   ptype_attributes }

  method type_kind acc = function
    | Ptype_abstract
    | Ptype_open _
      as tk -> acc, tk
    | Ptype_variant cds ->
      let acc, cds = loc (list self#constructor_declaration) acc cds in
      acc, Ptype_variant cds
    | Ptype_record lbls ->
      let acc, lbls = list self#label_declaration acc lbls in
      acc, Ptype_record lbls

  method label_declaration acc lbl =
    let acc, pld_type = self#core_type acc lbl.pld_type in
    let acc, pld_attributes = self#attributes acc lbl.pld_attributes in
    acc, { lbl with pld_type; pld_attributes }

  method constructor_declaration acc cd =
    let acc, pcd_args = self#constructor_arguments acc cd.pcd_args in
    let acc, pcd_res = option self#core_type acc cd.pcd_res in
    let acc, pcd_attributes = self#attributes acc cd.pcd_attributes in
    acc, { cd with pcd_args; pcd_res; pcd_attributes }

  method constructor_arguments acc = function
    | Pcstr_tuple cts ->
      let acc, cts = list self#core_type acc cts in
      acc, Pcstr_tuple cts
    | Pcstr_record lbls ->
      let acc, lbls = list self#label_declaration acc lbls in
      acc, Pcstr_record lbls

  method type_extension acc text =
    let acc, ptyext_params =
      list (fun acc (ct, v) ->
        let acc, ct = self#core_type acc ct in
        acc, (ct, v)
      ) acc text.ptyext_params
    in
    let acc, ptyext_constructors =
      list self#extension_constructor acc text.ptyext_constructors
    in
    let acc, ptyext_attributes = self#attributes acc text.ptyext_attributes in
    acc, { text with ptyext_params; ptyext_constructors; ptyext_attributes }

  method extension_constructor acc ec =
    let acc, pext_kind = self#extension_constructor_kind acc ec.pext_kind in
    let acc, pext_attributes = self#attributes acc ec.pext_attributes in
    acc, { ec with pext_kind; pext_attributes }

  method type_exception acc texn =
    let acc, ptyexn_constructor =
      self#extension_constructor acc texn.ptyexn_constructor
    in
    let acc, ptyexn_attributes = self#attributes acc texn.ptyexn_attributes in
    acc, { texn with ptyexn_constructor; ptyexn_attributes }

  method extension_constructor_kind acc = function
    | Pext_rebind _ as kind -> acc, kind
    | Pext_decl (ca, cto) ->
      let acc, ca = self#constructor_arguments acc ca in
      let acc, cto = option self#core_type acc cto in
      acc, Pext_decl (ca, cto)

  method class_type acc ct =
    let acc, pcty_desc = self#class_type_desc acc ct.pcty_desc in
    let acc, pcty_attributes = self#attributes acc ct.pcty_attributes in
    acc, { ct with pcty_desc; pcty_attributes }

  method class_type_desc acc = function
    | Pcty_constr (lid, cts) ->
      let acc, cts = list self#core_type acc cts in
      acc, Pcty_constr (lid, cts)
    | Pcty_signature csig ->
      let acc, csig = self#class_signature acc csig in
      acc, Pcty_signature csig
    | Pcty_arrow (arg, ct, clt) ->
      let acc, ct = self#core_type acc ct in
      let acc, clt = self#class_type acc clt in
      acc, Pcty_arrow (arg, ct, clt)
    | Pcty_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Pcty_extension ext
    | Pcty_open (od, clt) ->
      let acc, od = self#open_description acc od in
      let acc, clt = self#class_type acc clt in
      acc, Pcty_open (od, clt)

  method class_signature acc { pcsig_self; pcsig_fields } =
    let acc, pcsig_self = self#core_type acc pcsig_self in
    let acc, pcsig_fields =
      list self#class_type_field acc pcsig_fields
    in
    acc, { pcsig_self; pcsig_fields }

  method class_type_field acc ctf =
    let acc, pctf_desc = self#class_type_field_desc acc ctf.pctf_desc in
    let acc, pctf_attributes = self#attributes acc ctf.pctf_attributes in
    acc, { ctf with pctf_desc; pctf_attributes }

  method class_type_field_desc acc = function
    | Pctf_inherit ct ->
      let acc, ct = self#class_type acc ct in
      acc, Pctf_inherit ct
    | Pctf_val (lbl, mut, virt, ct) ->
      let acc, ct = self#core_type acc ct in
      acc, Pctf_val (lbl, mut, virt, ct)
    | Pctf_method  (lbl, priv, virt, ct) ->
      let acc, ct = self#core_type acc ct in
      acc, Pctf_method (lbl, priv, virt, ct)
    | Pctf_constraint (ct1, ct2) ->
      let acc, ct1 = self#core_type acc ct1 in
      let acc, ct2 = self#core_type acc ct2 in
      acc, Pctf_constraint (ct1, ct2)
    | Pctf_attribute attr ->
      let acc, attr = self#attribute acc attr in
      acc, Pctf_attribute attr
    | Pctf_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Pctf_extension ext

  method class_infos : 'a. ('acc -> 'a -> 'acc * 'a) -> 'acc ->
    'a class_infos -> 'acc * 'a class_infos =
    fun f_expr acc ci ->
    let acc, pci_params =
      list (fun acc (ct, v) ->
        let acc, ct = self#core_type acc ct in
        acc, (ct, v)
      ) acc ci.pci_params
    in
    let acc, pci_term_params = list self#fun_param acc ci.pci_term_params in
    let acc, pci_type = option self#class_type acc ci.pci_type in
    let acc, pci_expr = f_expr acc ci.pci_expr in
    let acc, pci_attributes = self#attributes acc ci.pci_attributes in
    acc, { ci with pci_params; pci_term_params; pci_type; pci_expr;
                   pci_attributes }

  method class_description = self#class_infos self#class_type

  method class_type_declaration = self#class_infos self#class_type

  method class_expr acc {pcl_desc;pcl_loc;pcl_attributes} =
    let acc, pcl_desc = self#class_expr_desc acc pcl_desc in
    let acc, pcl_attributes = self#attributes acc pcl_attributes in
    acc, {pcl_desc;pcl_loc;pcl_attributes}

  method class_expr_desc acc = function
    | Pcl_constr (lid, cts) ->
      let acc, cts = list self#core_type acc cts in
      acc, Pcl_constr (lid, cts)
    | Pcl_structure cstr ->
      let acc, cstr = self#class_structure acc cstr in
      acc, Pcl_structure cstr
    | Pcl_fun (ps, ce) ->
      let acc, ps = list self#fun_param acc ps in
      let acc, ce = self#class_expr acc ce in
      acc, Pcl_fun (ps, ce)
    | Pcl_apply (ce, args) ->
      let acc, ce = self#class_expr acc ce in
      let acc, args =
        list (fun acc (lbl, e) ->
          let acc, e = self#expression acc e in
          acc, (lbl, e)
        ) acc args
      in
      acc, Pcl_apply (ce, args)
    | Pcl_let (rf, vbs, ce) ->
      let acc, vbs = list self#value_binding acc vbs in
      let acc, ce = self#class_expr acc ce in
      acc, Pcl_let (rf, vbs, ce)
    | Pcl_constraint (ce, ct) ->
      let acc, ce = self#class_expr acc ce in
      let acc, ct = self#class_type acc ct in
      acc, Pcl_constraint (ce, ct)
    | Pcl_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Pcl_extension ext
    | Pcl_open (od, ce) ->
      let acc, od = self#open_description acc od in
      let acc, ce = self#class_expr acc ce in
      acc, Pcl_open (od, ce)

  method class_structure acc { pcstr_self; pcstr_fields } =
    let acc, pcstr_self = self#pattern acc pcstr_self in
    let acc, pcstr_fields = list self#class_field acc pcstr_fields in
    acc, { pcstr_self; pcstr_fields }

  method class_field acc cf =
    let acc, pcf_desc = self#class_field_desc acc cf.pcf_desc in
    let acc, pcf_attributes = self#attributes acc cf.pcf_attributes in
    acc, { cf with pcf_desc; pcf_attributes }

  method class_field_desc acc = function
    | Pcf_inherit (of_, ce, alias) ->
      let acc, ce = self#class_expr acc ce in
      acc, Pcf_inherit (of_, ce, alias)
    | Pcf_val (lbl, mut, cfk) ->
      let acc, cfk = self#class_field_kind acc cfk in
      acc, Pcf_val (lbl, mut, cfk)
    | Pcf_method (lbl, priv, cfk) ->
      let acc, cfk = self#class_field_kind acc cfk in
      acc, Pcf_method (lbl, priv, cfk)
    | Pcf_constraint (ct1, ct2) ->
      let acc, ct1 = self#core_type acc ct1 in
      let acc, ct2 = self#core_type acc ct2 in
      acc, Pcf_constraint (ct1, ct2)
    | Pcf_initializer e ->
      let acc, e = self#expression acc e in
      acc, Pcf_initializer e
    | Pcf_attribute attr ->
      let acc, attr = self#attribute acc attr in
      acc, Pcf_attribute attr
    | Pcf_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Pcf_extension ext

  method class_field_kind acc = function
    | Cfk_virtual ct ->
      let acc, ct = self#core_type acc ct in
      acc, Cfk_virtual ct
    | Cfk_concrete (of_, ps, (cto1, cto2), e) ->
      let acc, ps = list self#fun_param acc ps in
      let acc, cto1 = option self#core_type acc cto1 in
      let acc, cto2 = option self#core_type acc cto2 in
      let acc, e = self#expression acc e in
      acc, Cfk_concrete (of_, ps, (cto1, cto2), e)

  method class_declaration = self#class_infos self#class_expr 

  method module_type acc mty =
    let acc, pmty_desc = self#module_type_desc acc mty.pmty_desc in
    let acc, pmty_attributes = self#attributes acc mty.pmty_attributes in
    acc, { mty with pmty_desc; pmty_attributes }

  method module_type_desc acc = function
    | Pmty_ident _
    | Pmty_alias _
        as desc -> acc, desc
    | Pmty_signature sg ->
      let acc, sg = self#signature acc sg in
      acc, Pmty_signature sg
    | Pmty_functor (fps, mty) ->
      let acc, fps = list (loc self#functor_parameter) acc fps in
      let acc, mty = self#module_type acc mty in
      acc, Pmty_functor (fps, mty)
    | Pmty_with (mty, cstrs) ->
      let acc, mty = self#module_type acc mty in
      let acc, cstrs = list self#located_with_constraint acc cstrs in
      acc, Pmty_with (mty, cstrs)
    | Pmty_typeof me ->
      let acc, me = self#module_expr acc me in
      acc, Pmty_typeof me
    | Pmty_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Pmty_extension ext

  method functor_parameter acc = function
    | Unit -> acc, Unit
    | Named (name, mty) ->
      let acc, mty = self#module_type acc mty in
      acc, Named (name, mty)

  method signature = list self#signature_item

  method signature_item acc si =
    let acc, psig_desc = self#signature_item_desc acc si.psig_desc in
    acc, { si with psig_desc }

  method signature_item_desc acc = function
    | Psig_value vd ->
      let acc, vd = self#value_description acc vd in
      acc, Psig_value vd
    | Psig_type (rf, tds) ->
      let acc, tds =  list self#type_declaration acc tds in
      acc, Psig_type (rf, tds)
    | Psig_typesubst tds ->
      let acc, tds =  list self#type_declaration acc tds in
      acc, Psig_typesubst tds
    | Psig_typext text ->
      let acc, text = self#type_extension acc text in
      acc, Psig_typext text
    | Psig_exception exn ->
      let acc, exn = self#type_exception acc exn in
      acc, Psig_exception exn
    | Psig_module md ->
      let acc, md = self#module_declaration acc md in
      acc, Psig_module md
    | Psig_modsubst ms ->
      let acc, ms = self#module_substitution acc ms in
      acc, Psig_modsubst ms
    | Psig_recmodule mds ->
      let acc, mds = list self#module_declaration acc mds in
      acc, Psig_recmodule mds
    | Psig_modtype mtd ->
      let acc, mtd = self#module_type_declaration acc mtd in
      acc, Psig_modtype mtd
    | Psig_open od ->
      let acc, od = self#open_description acc od in
      acc, Psig_open od
    | Psig_include incl ->
      let acc, incl = self#include_description acc incl in
      acc, Psig_include incl
    | Psig_class cds ->
      let acc, cds = list self#class_description acc cds in
      acc, Psig_class cds
    | Psig_class_type cts ->
      let acc, cts = list self#class_type_declaration acc cts in
      acc, Psig_class_type cts
    | Psig_attribute attr ->
      let acc, attr = self#attribute acc attr in
      acc, Psig_attribute attr
    | Psig_extension (ext, attrs) ->
      let acc, ext = self#extension acc ext in
      let acc, attrs = self#attributes acc attrs in
      acc, Psig_extension (ext, attrs)

  method module_declaration acc md =
    let acc, pmd_params = list (loc self#functor_parameter) acc md.pmd_params in
    let acc, pmd_type = self#module_type acc md.pmd_type in
    let acc, pmd_attributes = self#attributes acc md.pmd_attributes in
    acc, { md with pmd_params; pmd_type; pmd_attributes }

  method module_substitution acc ms =
    let acc, pms_attributes = self#attributes acc ms.pms_attributes in
    acc, { ms with pms_attributes }

  method module_type_declaration acc mtd =
    let acc, pmtd_type = option self#module_type acc mtd.pmtd_type in
    let acc, pmtd_attributes = self#attributes acc mtd.pmtd_attributes in
    acc, { mtd with pmtd_type; pmtd_attributes }

  method open_infos : 'a. ('acc -> 'a -> 'acc * 'a) -> 'acc -> 'a open_infos ->
    'acc * 'a open_infos = fun f acc oi ->
    let acc, popen_expr = f acc oi.popen_expr in
    let acc, popen_attributes = self#attributes acc oi.popen_attributes in
    acc, { oi with popen_expr; popen_attributes }

  method open_description = self#open_infos (fun acc lid -> acc, lid)

  method open_declaration = self#open_infos self#module_expr 

  method include_infos : 'a. ('acc -> 'a -> 'acc * 'a) -> 'acc ->
    'a include_infos -> 'acc * 'a include_infos = fun f acc ii ->
    let acc, pincl_mod = f acc ii.pincl_mod in
    let acc, pincl_attributes = self#attributes acc ii.pincl_attributes in
    acc, { ii with pincl_mod; pincl_attributes }

  method include_description = self#include_infos self#module_type

  method include_declaration = self#include_infos self#module_expr 

  method located_with_constraint acc ((aow : Asttypes.and_or_with), wc) =
    let acc, wc = self#with_constraint acc wc in
    acc, (aow, wc)

  method with_constraint acc = function
    | Pwith_module _
    | Pwith_modsubst _
        as wc -> acc, wc
    | Pwith_type (l, td) ->
      let acc, td = self#type_declaration acc td in
      acc, Pwith_type (l, td)
    | Pwith_typesubst (l, td) ->
      let acc, td = self#type_declaration acc td in
      acc, Pwith_typesubst (l, td)

  method module_expr acc me =
    let acc, pmod_desc = self#module_expr_desc acc me.pmod_desc in
    let acc, pmod_attributes = self#attributes acc me.pmod_attributes in
    acc, { me with pmod_desc; pmod_attributes }

  method module_expr_desc acc = function
    | Pmod_ident _
        as desc -> acc, desc
    | Pmod_structure str ->
      let acc, str = self#structure acc str in
      acc, Pmod_structure str
    | Pmod_functor (ps, me) ->
      let acc, ps = list (loc self#functor_parameter) acc ps in
      let acc, me = self#module_expr acc me in
      acc, Pmod_functor (ps, me)
    | Pmod_apply (me1, me2) ->
      let acc, me1 = self#module_expr acc me1 in
      let acc, me2 = self#module_expr acc me2 in
      acc, Pmod_apply (me1, me2)
    | Pmod_constraint (me, mt) ->
      let acc, me = self#module_expr acc me in
      let acc, mt = self#module_type acc mt in
      acc, Pmod_constraint (me, mt)
    | Pmod_unpack e ->
      let acc, e = self#expression acc e in
      acc, Pmod_unpack e
    | Pmod_extension ext ->
      let acc, ext = self#extension acc ext in
      acc, Pmod_extension ext

  method structure = list self#structure_item 

  method structure_item acc si =
    let acc, pstr_desc = self#structure_item_desc acc si.pstr_desc in
    acc, { si with pstr_desc }

  method structure_item_desc acc = function
    | Pstr_eval (e, attrs) ->
      let acc, e = self#expression acc e in
      let acc, attrs = self#attributes acc attrs in
      acc, Pstr_eval (e, attrs)
    | Pstr_value (rf, vbs) ->
      let acc, vbs = list self#value_binding acc vbs in
      acc, Pstr_value (rf, vbs)
    | Pstr_primitive vd ->
      let acc, vd = self#value_description acc vd in
      acc, Pstr_primitive vd
    | Pstr_type (rf, tds) ->
      let acc, tds = list self#type_declaration acc tds in
      acc, Pstr_type (rf, tds)
    | Pstr_typext text ->
      let acc, text = self#type_extension acc text in
      acc, Pstr_typext text
    | Pstr_exception exn ->
      let acc, exn = self#type_exception acc exn in
      acc, Pstr_exception exn
    | Pstr_module mb ->
      let acc, mb = self#module_binding acc mb in
      acc, Pstr_module mb
    | Pstr_recmodule mbs ->
      let acc, mbs = list self#module_binding acc mbs in
      acc, Pstr_recmodule mbs
    | Pstr_modtype mtd ->
      let acc, mtd = self#module_type_declaration acc mtd in
      acc, Pstr_modtype mtd
    | Pstr_open od ->
      let acc, od = self#open_declaration acc od in
      acc, Pstr_open od
    | Pstr_class cds ->
      let acc, cds = list self#class_declaration acc cds in
      acc, Pstr_class cds
    | Pstr_class_type ctds ->
      let acc, ctds = list self#class_type_declaration acc ctds in
      acc, Pstr_class_type ctds
    | Pstr_include id ->
      let acc, id = self#include_declaration acc id in
      acc, Pstr_include id
    | Pstr_attribute attr ->
      let acc, attr = self#attribute acc attr in
      acc, Pstr_attribute attr
    | Pstr_extension (ext, attrs) ->
      let acc, ext = self#extension acc ext in
      let acc, attrs = self#attributes acc attrs in
      acc, Pstr_extension (ext, attrs)

  method value_binding acc vb =
    let acc, pvb_pat = self#pattern acc vb.pvb_pat in
    let acc, pvb_params = list self#fun_param acc vb.pvb_params in
    let acc, pvb_type =
      let (ct1, ct2) = vb.pvb_type in
      let acc, ct1 = option self#core_type acc ct1 in
      let acc, ct2 = option self#core_type acc ct2 in
      acc, (ct1, ct2)
    in
    let acc, pvb_expr = self#expression acc vb.pvb_expr in
    let acc, pvb_attributes = self#attributes acc vb.pvb_attributes in
    let acc, pvb_ext_attributes = 
      let (x, attrs) = vb.pvb_ext_attributes in
      let acc, attrs = self#attributes acc attrs in
      acc, (x, attrs)
    in
    acc, { vb with pvb_pat; pvb_params; pvb_type; pvb_expr; pvb_attributes;
                   pvb_ext_attributes }

  method module_binding acc mb =
    let acc, pmb_params = list (loc self#functor_parameter) acc mb.pmb_params in
    let acc, pmb_type = option self#module_type acc mb.pmb_type in
    let acc, pmb_expr = self#module_expr acc mb.pmb_expr in
    let acc, pmb_attributes = self#attributes acc mb.pmb_attributes in
    acc, { mb with pmb_params; pmb_type; pmb_expr; pmb_attributes }
end
(*
(** {1 Toplevel} *)

(* Toplevel phrases *)

type toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive
     (* #use, #load ... *)

and toplevel_directive =
  {
    pdir_name : string loc;
    pdir_arg : directive_argument option;
    pdir_loc : Location.t;
  }

and directive_argument =
  {
    pdira_desc : directive_argument_desc;
    pdira_loc : Location.t;
  }

and directive_argument_desc =
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of Longident.t
  | Pdir_bool of bool
end
   *)
