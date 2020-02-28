open Asttypes
open Parsetree
open PPrint

let prefix ~indent:n ~spaces:b l r = prefix n b l r

module Tokens = struct
  let pipe = char '|'

  let and_ = string "and"

  let as_ = string "as"

  let arrow = string "->"

  let exception_ = string "exception"

  let module_ = string "module"

  let of_ = string "of"

  let dotdot = string ".."

  let lazy_ = string "lazy"

  let let_ = string "let"

  let in_ = string "in"
end
open Tokens

module Longident : sig
  include module type of struct include Longident end

  val pp : Longident.t -> document
end = struct
  include Longident

  let rec pp = function
    | Lident s -> string s
    | Ldot (lid, s) -> pp lid ^^ dot ^^ break 0 ^^ string s
    | Lapply (l1, l2) -> pp l1 ^^ break 0 ^^ parens (pp l2)

  let pp lid = hang 2 (pp lid)
end

module Constant : sig
  val pp : constant -> document
end = struct
  let pp_string_lit s = arbitrary_string (String.escaped s)
  let pp_quoted_string ~delim s =
    let delim = string delim in
    braces (
      enclose (delim ^^ pipe) (pipe ^^ delim)
        (arbitrary_string s)
    )

  let pp = function
    | Pconst_float (nb, suffix_opt)
    | Pconst_integer (nb, suffix_opt) -> string nb ^^ optional char suffix_opt
    | Pconst_char c                   -> squotes (char c)
    | Pconst_string (s, None)         -> dquotes (pp_string_lit s)
    | Pconst_string (s, Some delim)   -> pp_quoted_string ~delim s
end

module Polymorphic_variant_tag : sig
  val pp : label -> document
end = struct
  let pp tag = bquote ^^ string tag
end

module rec Attribute : sig
  type kind =
    | Free_floating
    | Attached_to_structure_item
    | Attached_to_item

  val pp : kind -> attribute -> document
end = struct
  type kind =
    | Free_floating
    | Attached_to_structure_item
    | Attached_to_item

  let ats kind =
    string (
      match kind with
      | Free_floating -> "@@@"
      | Attached_to_structure_item -> "@@"
      | Attached_to_item -> "@"
    )

  let pp kind { attr_name; attr_payload; attr_loc = _ } =
    brackets (ats kind ^^ string attr_name.txt ^^ Payload.pp attr_payload)
end

and Extension : sig
  type kind =
    | Structure_item
    | Item

  val pp : kind -> extension -> document
end = struct
  type kind =
    | Structure_item
    | Item

  let percents kind =
    string (
      match kind with
      | Structure_item -> "%%"
      | Item -> "%"
    )

  let pp kind ({ Location.txt = ext_name; _ }, ext_payload) =
    brackets (percents kind ^^ string ext_name ^^ Payload.pp ext_payload)
end

and Payload : sig
  val pp : payload -> document
end = struct
  let pp = function
    | PStr _ -> nest 2 (break 1 ^^ string "TODO")
    | PSig _ -> nest 2 (colon ^/^ string "TODO")
    | PTyp _ -> nest 2 (colon ^/^ string "TODO")
    | PPat _ -> nest 2 (qmark ^/^ string "TODO")
end

and Core_type : sig
  val pp : ?needs_parens:bool -> core_type -> document
end = struct
  let pp_var v = squote ^^ string v

  let rec pp ?(needs_parens=false) ct =
    pp_desc ~needs_parens ct.ptyp_desc

  and pp_desc ~needs_parens = function
    | Ptyp_any -> underscore
    | Ptyp_var v -> pp_var v
    (* FIXME: use n-ary arrow *)
    | Ptyp_arrow (lbl, ct1, ct2) -> pp_arrow ~needs_parens lbl ct1 ct2
    | Ptyp_tuple lst -> pp_tuple ~needs_parens lst
    | Ptyp_constr (name, args) -> pp_constr name args
    | Ptyp_object (fields, closed) -> pp_object fields closed
    | Ptyp_class (name, args) -> pp_class name args
    | Ptyp_alias (ct, alias) -> pp_alias ~needs_parens ct alias
    | Ptyp_variant (fields, closed, present) -> pp_variant fields closed present
    | Ptyp_poly (vars, ct) -> pp_poly vars ct
    | Ptyp_package pkg -> pp_package pkg
    | Ptyp_extension ext -> Extension.pp Item ext

  and pp_arrow ~needs_parens arg_label ct1 ct2 =
    let lhs =
      let lhs = pp ct1 in
      match arg_label with
      | Nolabel -> lhs
      | Labelled l -> string l ^^ colon ^^ break 0 ^^ lhs
      | Optional l -> qmark ^^ string l ^^ colon ^^ break 0 ^^ lhs
    in
    let rhs = pp ct2 in
    let arrow = infix 2 1 lhs arrow rhs in
    if needs_parens then
      parens arrow
    else
      arrow

  and pp_tuple ~needs_parens lst =
    let tuple = separate_map star pp lst in
    if needs_parens then
      parens tuple
    else
      tuple

  and pp_constr name args =
    let name = Longident.pp name.txt in
    match args with
    | [] -> name
    | x :: xs -> pp_params x xs ^/^ name

  and pp_params first = function
    | []   -> pp first
    | rest -> parens (separate_map comma pp (first :: rest))

  and pp_object fields closed =
    let semi_sep = semi ^^ break 1 in
    let fields = separate_map semi_sep Object_field.pp fields in
    let fields =
      match closed with
      | Closed -> fields
      | Open -> fields ^^ semi_sep ^^ underscore
    in
    angles fields

  and pp_class name args =
    let name = sharp ^^ Longident.pp name.txt in
    match args with
    | [] -> name
    | x :: xs -> pp_params x xs ^/^ name

  (* FIXME: not sure parens are ever needed *)
  and pp_alias ~needs_parens ct alias =
    (* TODO: hang & ident one linebreak *)
    let alias = pp ct ^/^ as_ ^/^ pp_var alias in
    if needs_parens then
      parens alias
    else
      alias

  and pp_variant fields closed present =
        (* [ `A|`B ]         (flag = Closed; labels = None)
           [> `A|`B ]        (flag = Open;   labels = None)
           [< `A|`B ]        (flag = Closed; labels = Some [])
           [< `A|`B > `X `Y ](flag = Closed; labels = Some ["X";"Y"])
         *)
    let le_caca =
      match closed, present with
      | Closed, None ->
        (* FIXME: this will do the breaking randomly. Take inspiration from what
           was done in odoc. *)
        let sep = break 1 ^^ group (pipe ^^ break 1) in
        hang 0 (separate_map sep Row_field.pp fields)
      | _, _ ->
        (* FIXME *)
        assert false
    in
    group (
      brackets (
        le_caca
      )
    )

  and pp_poly vars ct =
    let ct = pp ct in
    match vars with
    | [] -> ct
    | vars ->
      prefix ~indent:2 ~spaces:1
        (* FIXME: do I need to group here? *)
        ((separate_map space (fun { Location.txt; _ } -> pp_var txt) vars) ^^ dot)
        ct

  and pp_package pkg =
    parens (module_ ^/^ Package_type.pp pkg)
end

and Object_field : sig
  val pp : object_field -> document
end = struct
  let pp_desc = function
    | Otag (name, ct) -> string name.txt ^^ colon ^/^ Core_type.pp ct
    | Oinherit ct -> Core_type.pp ct

  let pp { pof_desc; pof_attributes; _ } =
    let desc = pp_desc pof_desc in
    (* TODO: extract that logic *)
    match pof_attributes with
    | [] -> desc
    | attrs ->
      group (
        nest 2 (
          desc ^/^
          separate_map (break 0) (Attribute.pp Attached_to_item) attrs
        )
      )
end

and Package_type : sig
  val pp : package_type -> document
end = struct
  let pp _ = assert false
end

and Row_field : sig
  val pp : row_field -> document
end = struct
  let pp_desc = function
    | Rinherit ct -> Core_type.pp ct
    | Rtag (tag, true, []) -> Polymorphic_variant_tag.pp tag.txt
    | Rtag (tag, has_empty_constr, params) ->
      let sep = break 1 ^^ ampersand ^^ break 1 in
      let params = separate_map sep Core_type.pp params in
      let params =
        if has_empty_constr then
          sep ^^ params
        else
          break 1 ^^ params
      in
      Polymorphic_variant_tag.pp tag.txt ^/^ of_ ^^ params

  let pp { prf_desc; prf_attributes; _ } =
    let desc = pp_desc prf_desc in
    (* TODO: extract that logic *)
    match prf_attributes with
    | [] -> desc
    | attrs ->
      group (
        nest 2 (
          desc ^/^
          separate_map (break 0) (Attribute.pp Attached_to_item) attrs
        )
      )
end

and Pattern : sig
  val pp : pattern -> document
end = struct
  let rec pp { ppat_desc; ppat_attributes; _ } =
    let desc = pp_desc ppat_desc in
    match ppat_attributes with
    | [] -> desc
    | attrs ->
      group (
        nest 2 (
          desc ^/^
          separate_map (break 0) (Attribute.pp Attached_to_item) attrs
        )
      )

  and pp_alias pat alias =
    nest 2 (pp pat ^/^ as_ ^/^ string alias.txt)

  and pp_interval c1 c2 =
    Constant.pp c1 ^/^ dotdot ^/^ Constant.pp c2

  (* FIXME? nest on the outside, not in each of them. *)

  and pp_tuple lst =
    nest 2 (
      separate_map (star ^^ break 1) pp lst
    )

  and pp_construct name arg_opt =
    let name = Longident.pp name.txt in
    match arg_opt with
    | None -> name
    | Some p -> prefix ~indent:2 ~spaces:1 name (parens (pp p))

  and pp_variant tag arg_opt =
    let tag = Polymorphic_variant_tag.pp tag in
    match arg_opt with
    | None -> tag
    | Some p -> 
      let arg = pp p in
      let arg =
        match p.ppat_desc with
        | Ppat_tuple _ -> parens arg
        | _ -> arg
      in
      tag ^/^ arg

  and pp_record_field (lid, pat) =
    (* TODO: print the whole lid only once *)
    let field = Longident.pp lid.txt in
    match pat.ppat_desc with
    | Ppat_var v when Longident.last lid.txt = v.txt -> field
    | _ -> field ^/^ equals ^/^ pp pat

  and pp_record pats closed =
    let fields =
      separate_map (semi ^^ break 1)
        pp_record_field pats
    in
    let fields =
      match closed with
      | Closed -> fields
      | Open -> fields ^^ semi ^/^ underscore
    in
    braces fields

  and pp_array pats =
    brackets (
      pipe ^/^
      separate_map (semi ^^ break 1) pp pats ^/^
      pipe
    )

  and pp_or p1 p2 =
    pp p1 ^^ hardline ^^ pipe ^^ space ^^ pp p2

  and pp_constraint p ct =
    match p.ppat_desc with
    | Ppat_unpack mod_name -> pp_unpack mod_name (Some ct)
    | _ -> parens (pp p ^/^ colon ^/^ Core_type.pp ct)

  and pp_type typ =
    sharp ^^ Longident.pp typ.txt

  and pp_lazy p =
    (* FIXME: remove parens when not needed. *)
    parens (lazy_ ^/^  pp p)

  and pp_unpack mod_name ct =
    let constraint_ =
      match ct with
      | None -> empty
      | Some ct ->
        match ct.ptyp_desc with
        | Ptyp_package pkg -> break 1 ^^ colon ^/^ Package_type.pp pkg
        | _ -> assert false
    in
    parens (module_ ^/^ string mod_name.txt ^^ constraint_)

  and pp_exception p =
    (* FIXME: needs parens *)
    exception_ ^/^ pp p

  and pp_open lid_loc p =
    Longident.pp lid_loc.txt ^^ dot ^^ parens (break 0 ^^ pp p)

  and pp_desc = function
    | Ppat_any -> underscore
    | Ppat_var v -> string v.txt
    | Ppat_alias (pat, alias) -> pp_alias pat alias
    | Ppat_constant c -> Constant.pp c
    | Ppat_interval (c1, c2) -> pp_interval c1 c2
    | Ppat_tuple pats -> pp_tuple pats
    | Ppat_construct (name, arg) -> pp_construct name arg
    | Ppat_variant (tag, arg) -> pp_variant tag arg
    | Ppat_record (pats, closed) -> pp_record pats closed
    | Ppat_array pats -> pp_array pats
    | Ppat_or (p1, p2) -> pp_or p1 p2
    | Ppat_constraint (p, ct) -> pp_constraint p ct
    | Ppat_type pt -> pp_type pt
    | Ppat_lazy p -> pp_lazy p
    | Ppat_unpack mod_name -> pp_unpack mod_name None
    | Ppat_exception p -> pp_exception p
    | Ppat_extension ext -> Extension.pp Item ext
    | Ppat_open (lid, p) -> pp_open lid p
end

and Expression : sig
  val pp : ?needs_parens:bool -> expression -> document
end = struct
  let rec pp ?(needs_parens=false) { pexp_desc; pexp_attributes; _ } =
    let desc = pp_desc ~needs_parens pexp_desc in
    match pexp_attributes with
    | [] -> desc
    | attrs ->
      group (
        nest 2 (
          desc ^/^
          separate_map (break 0) (Attribute.pp Attached_to_item) attrs
        )
      )

  and pp_desc ~needs_parens = function
    | Pexp_ident id -> Longident.pp id.txt
    | Pexp_constant c -> Constant.pp c
    | Pexp_let (rf, vbs, body) -> pp_let rf vbs body
    | Pexp_function cases -> pp_function ~needs_parens cases
    | Pexp_fun (lbl, default, pat, exp) ->
      pp_fun ~needs_parens lbl default pat exp
    | Pexp_apply (expr, args) -> pp_apply ~needs_parens expr args
    | Pexp_match (arg, cases) -> pp_match ~needs_parens arg cases
    | Pexp_try (arg, cases) -> pp_try ~needs_parens arg cases
    | Pexp_tuple exps -> pp_tuple ~needs_parens exps
    | Pexp_construct (lid, arg) -> pp_construct ~needs_parens lid arg
    | Pexp_variant (tag, arg) -> pp_variant ~needs_parens tag arg
    | Pexp_record (fields, exp) -> pp_record fields exp
    | _ -> assert false

  and pp_let rf vbs body =
    let vbs =
      List.mapi (fun i vb ->
        let lhs, rhs = Value_binding.pp Attached_to_item vb in
        let kw = if i = 0 then let_ ^^ rec_flag rf else and_ in
        prefix ~indent:2 ~spaces:1
          (group (prefix ~indent:2 ~spaces:1 kw (group (lhs ^/^ equals))))
          rhs
      ) vbs
    in
    let vbs = separate hardline vbs in
    let body = pp body in
    vbs ^/^ in_ ^/^ body

  and rec_flag = function
    | Recursive -> string " rec"
    | Nonrecursive -> empty

  and case { pc_lhs; pc_guard; pc_rhs } =
    let lhs = Pattern.pp pc_lhs in
    let rhs = pp pc_rhs in
    let lhs =
      match pc_guard with
      | None -> lhs
      | Some guard ->
        let guard = pp guard in
        lhs ^/^ group (!^"when" ^/^ guard)
    in
    prefix ~indent:2 ~spaces:1 (lhs ^/^ arrow) rhs

  and cases case_list =
    let cases = separate_map (break 1 ^^ pipe ^^ space) case case_list in
    let prefix = ifflat empty (hardline ^^ pipe) in
    prefix ^^ space ^^ cases

  and pp_function ~needs_parens case_list =
    let doc = !^"function" ^/^ cases case_list in
    if needs_parens then
      parens doc
    else
      doc

  and parameter lbl default pat =
    let suffix lbl =
      match pat.ppat_desc with
      | Ppat_var v when lbl = v.txt -> empty
      | _ -> colon ^^ Pattern.pp (* FIXME: needs parens = true *) pat
    in
    match lbl with
    | Nolabel -> Pattern.pp pat
    | Labelled lbl -> tilde ^^ string lbl ^^ suffix lbl
    | Optional lbl ->
      match default with
      | None -> qmark ^^ string lbl ^^ suffix lbl
      | Some def ->
        (* TODO: punning *)
        qmark ^^ string lbl ^^ colon ^^
        parens (group (Pattern.pp pat ^/^ equals ^/^ pp def))


  and pp_fun ~needs_parens lbl default pat exp =
    let body = pp exp in
    let arg = parameter lbl default pat in
    let doc =
      prefix ~indent:2 ~spaces:1
        (!^"fun" ^/^ arg ^/^ arrow)
        body
    in
    if needs_parens then
      parens doc
    else
      doc

  and argument (lbl, exp) =
    let suffix lbl =
      match exp.pexp_desc with
      | Pexp_ident { txt = Lident id; _ } when lbl = id -> empty
      | _ -> colon ^^ pp ~needs_parens:true exp
    in
    match lbl with
    | Nolabel -> pp ~needs_parens:true exp
    | Labelled lbl -> tilde ^^ string lbl ^^ suffix lbl
    | Optional lbl -> qmark ^^ string lbl ^^ suffix lbl

  and pp_apply ~needs_parens exp args =
    (* FIXME: handle infix ops *)
    let exp = pp ~needs_parens:true exp in
    let args = separate_map (break 1) argument args in
    let doc = prefix ~indent:2 ~spaces:1 exp args in
    if needs_parens then
      parens doc
    else
      doc

  and pp_match ~needs_parens arg case_list =
    let arg = pp arg in
    let cases = cases case_list in
    let doc =
      group (
        string "match" ^^
        nest 2 (break 1 ^^ arg) ^/^
        string "with"
      ) ^^ cases
    in
    if needs_parens then
      parens doc
    else
      doc

  and pp_try ~needs_parens arg case_list =
    let arg = pp arg in
    let cases = cases case_list in
    let doc =
      (* FIXME: the layout is generally different. *)
      group (
        string "try" ^^
        nest 2 (break 1 ^^ arg) ^/^
        string "with"
      ) ^^ cases
    in
    if needs_parens then
      parens doc
    else
      doc

  and pp_tuple ~needs_parens exps =
    let doc = separate_map (comma ^^ break 1) (pp ~needs_parens:true) exps in
    if needs_parens then
      parens doc
    else
      doc

  and pp_construct ~needs_parens lid arg_opt =
    let name = Longident.pp lid.txt in
    let arg  = optional (pp ~needs_parens:true) arg_opt in
    let doc  = prefix ~indent:2 ~spaces:1 name arg in
    if needs_parens then
      parens doc
    else
      doc

  and pp_variant ~needs_parens tag arg_opt =
    let tag = Polymorphic_variant_tag.pp tag in
    let arg  = optional (pp ~needs_parens:true) arg_opt in
    let doc  = prefix ~indent:2 ~spaces:1 tag arg in
    if needs_parens then
      parens doc
    else
      doc

  and record_field (lid, exp) =
    (* TODO: print the whole lid only once *)
    let fld = Longident.pp lid.txt in
    match exp.pexp_desc with
    | Pexp_ident { txt = Lident id; _ } when Longident.last lid.txt = id -> fld
    | _ -> fld ^/^ equals ^/^ pp exp

  and pp_record fields updated_record =
    let fields = separate_map (semi ^^ break 1) record_field fields in
    let prefix =
      match updated_record with
      | None -> empty
      | Some e -> pp ~needs_parens:true e ^/^ !^"with" ^^ break 1
    in
    braces (prefix ^^ fields)
end

and Value_binding : sig
  val pp : Attribute.kind -> value_binding -> document * document
end = struct
  let pp attr_kind { pvb_pat; pvb_expr; pvb_attributes; _ } =
    let lhs = Pattern.pp pvb_pat in
    let rhs = Expression.pp pvb_expr in
    let attrs =
      separate_map (break 0) (Attribute.pp attr_kind) pvb_attributes
    in
    lhs, rhs ^/^ attrs (* FIXME: is this correct? *)
end
