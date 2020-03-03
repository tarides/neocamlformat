open Asttypes
open Source_tree
open PPrint

let prefix ~indent:n ~spaces:b l r = prefix n b l r

let left_assoc_map ~sep ~f = function
  | [] -> empty
  | x :: xs ->
    List.fold_left (fun doc elt -> doc ^/^ group (sep ^^ f elt)) (f x) xs

module Binding = struct
  type t = {
    lhs : document;
    typ : document option;
    rhs : document;
  }

  let simple ~binder kw lhs rhs =
    prefix ~indent:2 ~spaces:1
      (group (prefix ~indent:2 ~spaces:1 kw (group (lhs ^/^ binder))))
      rhs

  let with_constraint ~binder kw lhs typ rhs =
    nest 2 (
      group (kw ^/^ (lhs ^^ space ^^ colon)) ^/^
      typ
    ) ^/^ binder ^^
    nest 2 (
      break 1 ^^ 
      rhs
    )

  let pp ?(binder=equals) ~keyword { lhs; rhs; typ } =
    match typ with
    | None -> simple ~binder keyword lhs rhs
    | Some typ -> with_constraint ~binder keyword lhs typ rhs
end

let module_name = function
  | None -> underscore
  | Some name -> string name

module Tokens = struct
  let pipe = char '|'

  let and_ = string "and"

  let as_ = string "as"

  let arrow = string "->"

  let larrow = string "<-"

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

  let pp_ident s =
    match String.get s 0 with
    | '!' | '?' | '~'
    | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|'
    | '%' | '<' | '#' -> parens (string s)
    | _ -> string s

  let rec pp = function
    | Lident s -> pp_ident s
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

  val attach : kind -> document -> attributes -> document
  val attach_to_item : document -> attributes -> document
  val attach_to_top_item : document -> attributes -> document
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

  let attach kind doc = function
    | [] -> doc
    | attrs ->
      group (
        prefix ~indent:2 ~spaces:1 doc 
          (separate_map (break 0) (pp kind) attrs)
      )

  let attach_to_item doc =
    attach Attached_to_item doc

  let attach_to_top_item doc =
    attach Attached_to_structure_item doc
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
    | PStr st -> nest 2 (break 1 ^^ Structure.pp st)
    | PSig sg -> nest 2 (colon ^/^ Signature.pp sg)
    | PTyp ct -> nest 2 (colon ^/^ Core_type.pp [] ct)
    | PPat (p, None) -> nest 2 (qmark ^/^ Pattern.pp [] p)
    | PPat (p, Some e) ->
      nest 2 (
        qmark ^/^ Pattern.pp [] p ^/^
        group (!^"when" ^/^ Expression.pp [] e)
      )
end

and Core_type : sig
  val pp : Printing_stack.t -> core_type -> document
end = struct
  let pp_var v = squote ^^ string v

  let rec pp ps ct =
    let ps = Printing_stack.Core_type ct.ptyp_desc :: ps in
    group (pp_desc ps ct.ptyp_desc)

  and pp_desc ps = function
    | Ptyp_any -> underscore
    | Ptyp_var v -> pp_var v
    (* FIXME: use n-ary arrow *)
    | Ptyp_arrow (params, ct2) -> pp_arrow ps params ct2
    | Ptyp_tuple lst -> pp_tuple ps lst
    | Ptyp_constr (name, args) -> pp_constr name args
    | Ptyp_object (fields, closed) -> pp_object fields closed
    | Ptyp_class (name, args) -> pp_class name args
    | Ptyp_alias (ct, alias) -> pp_alias ps ct alias
    | Ptyp_variant (fields, closed, present) -> pp_variant fields closed present
    | Ptyp_poly (vars, ct) -> pp_poly vars ct
    | Ptyp_package pkg -> pp_package pkg
    | Ptyp_extension ext -> Extension.pp Item ext

  and pp_param ps (arg_label, ct) =
    let ct = pp ps ct in
    match arg_label with
    | Nolabel -> ct
    | Labelled l -> string l ^^ colon ^^ break 0 ^^ ct
    | Optional l -> qmark ^^ string l ^^ colon ^^ break 0 ^^ ct

  and pp_arrow ps params res =
    let params =
      left_assoc_map ~sep:(arrow ^^ space) ~f:(pp_param ps) params
    in
    let res = pp (List.tl ps) res in
    let doc = params ^/^ group (arrow ^/^ res) in
    Printing_stack.parenthesize ps doc

  and pp_tuple ps l =
    let tuple = left_assoc_map ~sep:(star ^^ break 1) ~f:(pp ps) l in
    Printing_stack.parenthesize ps tuple

  and pp_constr name args =
    let name = Longident.pp name.txt in
    match args with
    | [] -> name
    | x :: xs -> pp_params x xs ^/^ name

  and pp_params first = function
    | []   -> pp [] first
    | rest -> parens (separate_map comma (pp []) (first :: rest))

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
  and pp_alias ps ct alias =
    (* TODO: hang & ident one linebreak *)
    let alias = pp ps ct ^/^ as_ ^/^ pp_var alias in
    Printing_stack.parenthesize ps alias

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
    (* FIXME: doesn't look right. *)
    let ct = pp [] ct in
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
    | Otag (name, ct) -> string name.txt ^^ colon ^/^ Core_type.pp [] ct
    | Oinherit ct -> Core_type.pp [] ct

  let pp { pof_desc; pof_attributes; _ } =
    let desc = pp_desc pof_desc in
    Attribute.attach_to_item desc pof_attributes
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
    | Rinherit ct -> Core_type.pp [] ct
    | Rtag (tag, true, []) -> Polymorphic_variant_tag.pp tag.txt
    | Rtag (tag, has_empty_constr, params) ->
      let sep = break 1 ^^ ampersand ^^ break 1 in
      let params = separate_map sep (Core_type.pp [ Row_field ]) params in
      let params =
        if has_empty_constr then
          sep ^^ params
        else
          break 1 ^^ params
      in
      Polymorphic_variant_tag.pp tag.txt ^/^ of_ ^^ params

  let pp { prf_desc; prf_attributes; _ } =
    let desc = pp_desc prf_desc in
    Attribute.attach_to_item desc prf_attributes
end

and Pattern : sig
  val pp : Printing_stack.t -> pattern -> document
end = struct
  let rec pp ps { ppat_desc; ppat_attributes; _ } =
    let ps = Printing_stack.Pattern ppat_desc :: ps in
    let desc = pp_desc ps ppat_desc in
    Attribute.attach_to_item desc ppat_attributes

  and pp_alias ps pat alias =
    nest 2 (pp ps pat ^/^ as_ ^/^ string alias.txt)

  and pp_interval c1 c2 =
    Constant.pp c1 ^/^ dotdot ^/^ Constant.pp c2

  (* FIXME? nest on the outside, not in each of them. *)

  and pp_tuple ps lst =
    let doc =
      nest 2 (
        separate_map (comma ^^ break 1) (pp ps) lst
      )
    in
    Printing_stack.parenthesize ps doc

  and pp_construct ps name arg_opt =
    let name = Longident.pp name.txt in
    match arg_opt with
    | None -> name
    | Some p ->
      let doc = prefix ~indent:2 ~spaces:1 name (pp ps p) in
      Printing_stack.parenthesize ps doc

  and pp_variant ps tag arg_opt =
    let tag = Polymorphic_variant_tag.pp tag in
    match arg_opt with
    | None -> tag
    | Some p -> 
      let arg = pp ps p in
      Printing_stack.parenthesize ps (tag ^/^ arg)

  and pp_record_field ps (lid, pat) =
    (* TODO: print the whole lid only once *)
    let field = Longident.pp lid.txt in
    match pat.ppat_desc with
    | Ppat_var v when Longident.last lid.txt = v.txt -> field
    | _ -> field ^/^ equals ^/^ pp ps pat

  and pp_record ps pats closed =
    let fields =
      separate_map (semi ^^ break 1)
        (pp_record_field ps) pats
    in
    let fields =
      match closed with
      | Closed -> fields
      | Open -> fields ^^ semi ^/^ underscore
    in
    braces fields

  and pp_array ps pats =
    brackets (
      pipe ^/^
      separate_map (semi ^^ break 1) (pp ps) pats ^/^
      pipe
    )

  and pp_or ps p1 p2 =
    let p1 =
      (* or-patterns are left-assoc *)
      pp (List.tl ps) p1
    in
    let p2 = pp ps p2 in
    let or_ = p1 ^/^ pipe ^^ space ^^ p2 in
    Printing_stack.parenthesize ps or_

  and pp_constraint p ct =
    match p.ppat_desc with
    | Ppat_unpack mod_name -> pp_unpack mod_name (Some ct)
    | _ ->
      (* Are there cases where we don't want parentheses? *)
      parens (pp [] p ^/^ colon ^/^ Core_type.pp [] ct)

  and pp_type typ =
    sharp ^^ Longident.pp typ.txt

  and pp_lazy ps p =
    Printing_stack.parenthesize ps (lazy_ ^/^  pp ps p)

  and pp_unpack mod_name ct =
    let constraint_ =
      match ct with
      | None -> empty
      | Some ct ->
        match ct.ptyp_desc with
        | Ptyp_package pkg -> break 1 ^^ colon ^/^ Package_type.pp pkg
        | _ -> assert false
    in
    parens (module_ ^/^ module_name mod_name.txt ^^ constraint_)

  and pp_exception ps p =
    exception_ ^/^ pp ps p

  and pp_open lid_loc p =
    Longident.pp lid_loc.txt ^^ dot ^^ parens (break 0 ^^ pp [] p)

  and pp_desc ps = function
    | Ppat_any -> underscore
    | Ppat_var v -> string v.txt
    | Ppat_alias (pat, alias) -> pp_alias ps pat alias
    | Ppat_constant c -> Constant.pp c
    | Ppat_interval (c1, c2) -> pp_interval c1 c2
    | Ppat_tuple pats -> pp_tuple ps pats
    | Ppat_construct (name, arg) -> pp_construct ps name arg
    | Ppat_variant (tag, arg) -> pp_variant ps tag arg
    | Ppat_record (pats, closed) -> pp_record ps pats closed
    | Ppat_array pats -> pp_array ps pats
    | Ppat_or (p1, p2) -> pp_or ps p1 p2
    | Ppat_constraint (p, ct) -> pp_constraint p ct
    | Ppat_type pt -> pp_type pt
    | Ppat_lazy p -> pp_lazy ps p
    | Ppat_unpack mod_name -> pp_unpack mod_name None
    | Ppat_exception p -> pp_exception ps p
    | Ppat_extension ext -> Extension.pp Item ext
    | Ppat_open (lid, p) -> pp_open lid p
end

and Application : sig
  val pp : Printing_stack.t -> expression -> (arg_label * expression) list ->
    document
end = struct
  let argument ps (lbl, exp) =
    let suffix lbl =
      match exp.pexp_desc with
      | Pexp_ident { txt = Lident id; _ } when lbl = id -> empty
      | _ -> colon ^^ Expression.pp ps exp
    in
    match lbl with
    | Nolabel -> Expression.pp ps exp
    | Labelled lbl -> tilde ^^ string lbl ^^ suffix lbl
    | Optional lbl -> qmark ^^ string lbl ^^ suffix lbl

  (* FIXME: handle infix ops *)
  let simple_apply ps exp args =
    let exp = Expression.pp ps exp in
    let args = separate_map (break 1) (argument ps) args in
    let doc = prefix ~indent:2 ~spaces:1 exp args in
    Printing_stack.parenthesize ps doc

  let prefix_op ps (exp, op) = function
    | (Nolabel, fst_arg) :: args ->
      let op = string op in
      let fst_arg = Expression.pp ps fst_arg in
      let args = separate_map (break 1) (argument ps) args in
      let doc = prefix ~indent:2 ~spaces:1 (op ^^ fst_arg) args in
      Printing_stack.parenthesize ps doc
    | args ->
      simple_apply ps exp args

  let infix_op ps (exp, op) = function
    | [ (Nolabel, fst); (Nolabel, snd) ] ->
      let op = string op in
      let fst = Expression.pp ps fst in
      let snd = Expression.pp ps snd in
      let doc = infix 2 1 op fst snd in
      Printing_stack.parenthesize ps doc
    | args ->
      simple_apply ps exp args

  (* TODO: precedence *)
  type kind = Prefix_op of string | Infix_op of string | Normal

  let classify_fun exp =
    match exp.pexp_desc with
    | Pexp_ident { txt = Lident s; _ } when s <> "" -> begin
        match String.get s 0 with
        | '!' | '?' | '~' -> Prefix_op s
        | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|'
        | '%' | '<' | '#' -> Infix_op s
        | _ -> Normal
      end
    | _ -> Normal

  let pp ps exp args =
    match classify_fun exp with
    | Normal -> simple_apply ps exp args
    | Prefix_op op -> prefix_op ps (exp, op) args
    | Infix_op op -> infix_op ps (exp, op) args

end

and Expression : sig
  val pp : Printing_stack.t -> expression -> document
end = struct
  let rec pp ps { pexp_desc; pexp_attributes; _ } =
    let desc = pp_desc (Printing_stack.Expression pexp_desc :: ps) pexp_desc in
    Attribute.attach_to_item desc pexp_attributes

  and pp_desc ps = function
    | Pexp_ident id -> pp_ident id
    | Pexp_constant c -> Constant.pp c
    | Pexp_let (rf, vbs, body) -> pp_let ps rf vbs body
    | Pexp_function cases -> pp_function ps cases
    | Pexp_fun (params, exp) ->
      pp_fun ps params exp
    | Pexp_apply (expr, args) -> Application.pp ps expr args
    | Pexp_match (arg, cases) -> pp_match ps arg cases
    | Pexp_try (arg, cases) -> pp_try ps arg cases
    | Pexp_tuple exps -> pp_tuple ps exps
    | Pexp_construct (lid, arg) -> pp_construct ps lid arg
    | Pexp_variant (tag, arg) -> pp_variant ps tag arg
    | Pexp_record (fields, exp) -> pp_record ps fields exp
    | Pexp_field (exp, fld) -> pp_field ps exp fld
    | Pexp_setfield (exp, fld, val_) -> pp_setfield ps exp fld val_
    | Pexp_array elts -> pp_array ps elts
    | Pexp_ifthenelse (cond, then_, else_) ->
      pp_if_then_else ps cond then_ else_
    | Pexp_sequence (e1, e2) -> pp_sequence ps e1 e2
    | Pexp_while (cond, body) -> pp_while cond body
    | Pexp_for (it, start, stop, dir, body) -> pp_for it start stop dir body
    | Pexp_constraint (e, ct) -> pp_constraint e ct
    | Pexp_coerce (e, ct_start, ct) -> pp_coerce e ct_start ct
    | Pexp_send (e, meth) -> pp_send ps e meth
    | Pexp_new lid -> pp_new lid
    | Pexp_setinstvar (lbl, exp) -> pp_setinstvar ps lbl exp
    | Pexp_override fields -> pp_override fields
    | Pexp_letmodule (name, mb, body) -> pp_letmodule ps name mb body
    | Pexp_letexception (exn, exp) -> pp_letexception exn exp
    | Pexp_assert exp -> pp_assert ps exp
    | Pexp_lazy exp -> pp_lazy ps exp
    | Pexp_object cl -> pp_object cl
    | Pexp_pack me -> pp_pack me None
    | Pexp_open (lid, exp) -> pp_open lid exp
    | Pexp_letopen (od, exp) -> pp_letopen ps od exp
    | Pexp_letop letop -> pp_letop letop
    | Pexp_extension ext -> Extension.pp Item ext
    | Pexp_unreachable -> dot

  and pp_ident id =
    (* FIXME: move the grouping to [Longident.pp] *)
    group (Longident.pp id.txt)

  and pp_let ps rf vbs body =
    let vbs =
      List.mapi (fun i vb ->
        let lhs, rhs = Value_binding.pp Attached_to_item vb in
        let keyword = if i = 0 then let_ ^^ rec_flag rf else and_ in
        Binding.pp ~keyword { lhs; rhs; typ = None }
      ) vbs
    in
    let vbs = separate hardline vbs in
    let body =
      let ps = if Printing_stack.will_parenthesize ps then [] else List.tl ps in
      pp ps body
    in
    Printing_stack.parenthesize ps (group (vbs ^/^ in_) ^/^ body)

  and rec_flag = function
    | Recursive -> string " rec"
    | Nonrecursive -> empty

  and case ps { pc_lhs; pc_guard; pc_rhs } =
    let lhs = Pattern.pp [] pc_lhs in
    let rhs = pp ps pc_rhs in
    let lhs =
      match pc_guard with
      | None -> lhs
      | Some guard ->
        let guard = pp ps guard in
        lhs ^/^ group (!^"when" ^/^ guard)
    in
    prefix ~indent:2 ~spaces:1 (group (lhs ^/^ arrow)) rhs

  and cases ps case_list =
    let cases = separate_map (break 1 ^^ pipe ^^ space) (case ps) case_list in
    let prefix = ifflat empty (hardline ^^ pipe) in
    prefix ^^ space ^^ cases

  and pp_function ps case_list =
    let doc = !^"function" ^^ (cases ps) case_list in
    Printing_stack.parenthesize ps doc

  and fun_ ~args ~body =
    prefix ~indent:2 ~spaces:1
      (group ((prefix ~indent:2 ~spaces:1 !^"fun" args) ^/^ arrow))
      body

  and pp_fun ps params exp =
    let body = pp ps exp in
    let args = left_assoc_map ~sep:empty ~f:Fun_param.pp params in
    let doc = fun_ ~args ~body in
    Printing_stack.parenthesize ps doc

  and pp_match ps arg case_list =
    let arg = pp [] arg in
    let cases = cases ps case_list in
    let doc =
      group (
        string "match" ^^
        nest 2 (break 1 ^^ arg) ^/^
        string "with"
      ) ^^ cases
    in
    Printing_stack.parenthesize ps doc

  and pp_try ps arg case_list =
    let arg = pp [] arg in
    let cases = cases ps case_list in
    let doc =
      (* FIXME: the layout is generally different. *)
      group (
        string "try" ^^
        nest 2 (break 1 ^^ arg) ^/^
        string "with"
      ) ^^ cases
    in
    Printing_stack.parenthesize ps doc

  and pp_tuple ps exps =
    let doc =
      group (separate_map (comma ^^ break 1) (pp ps) exps)
    in
    Printing_stack.parenthesize ps doc

  and pp_construct ps lid arg_opt =
    let name = Longident.pp lid.txt in
    let arg  = optional (pp ps) arg_opt in
    let doc  = prefix ~indent:2 ~spaces:1 name arg in
    Printing_stack.parenthesize ps doc

  and pp_variant ps tag arg_opt =
    let tag = Polymorphic_variant_tag.pp tag in
    let arg  = optional (pp ps) arg_opt in
    let doc  = prefix ~indent:2 ~spaces:1 tag arg in
    Printing_stack.parenthesize ps doc

  and record_field (lid, exp) =
    (* TODO: print the whole lid only once *)
    let fld = Longident.pp lid.txt in
    match exp.pexp_desc with
    | Pexp_ident { txt = Lident id; _ } when Longident.last lid.txt = id -> fld
    | _ -> fld ^/^ equals ^/^ pp [ Printing_stack.Record_field ] exp

  and pp_record ps fields updated_record =
    let fields = separate_map (semi ^^ break 1) record_field fields in
    let prefix =
      match updated_record with
      | None -> empty
      | Some e -> pp ps e ^/^ !^"with" ^^ break 1
    in
    braces (prefix ^^ fields)

  and pp_field ps re fld =
    let record = pp ps re in
    let field = Longident.pp fld.txt in
    flow (break 0) [
      record; dot; field
    ]

  and pp_setfield ps re fld val_ =
    let field = pp_field ps re fld in
    let value = pp (List.tl ps) val_ in
    prefix ~indent:2 ~spaces:1
      (group (field ^/^ larrow))
      value

  and pp_array ps elts =
    let elements =
      separate_map
        (semi ^^ break 1)
        (pp ps)
        elts
    in
    (* FIXME: empty arrays will have two spaces. *)
    brackets (pipe ^/^ elements ^/^ pipe)

  (* FIXME: change ast to present n-ary [if]s *)
  and pp_if_then_else ps cond then_ else_opt =
    let cond = pp [] cond in
    let then_ = pp ps then_ in
    let else_ =
      optional (fun e ->
        break 1 ^^ !^"else" ^^
        nest 2 (break 1 ^^ pp ps e)
      ) else_opt
    in
    group (
      group (
        string "if" ^^
        nest 2 (break 1 ^^ cond) ^/^
        string "then"
      ) ^^ 
      nest 2 (break 1 ^^ then_) ^^
      else_
    )

  and pp_sequence ps e1 e2 =
    let e1 = pp ps e1 in
    let e2 =
      let ps = if Printing_stack.will_parenthesize ps then [] else List.tl ps in
      pp ps e2
    in
    let doc = e1 ^^ semi ^/^ e2 in
    Printing_stack.parenthesize ps doc

  and pp_while cond body =
    let cond = pp [] cond in
    let body = pp [] body in
    group (
      group (
        string "while" ^^
        nest 2 (break 1 ^^ cond) ^/^
        string "do"
      ) ^^
      nest 2 (break 1 ^^ body) ^/^
      string "done"
    ) 

  and pp_for it start stop dir body =
    let it = Pattern.pp [ Printing_stack.Value_binding ] it in
    let start = pp [] start in
    let stop = pp [] stop in
    let dir =
      match dir with
      | Upto -> !^"to"
      | Downto -> !^"downto"
    in
    let body = pp [] body in
    group (
      group (
        string "for" ^^
        nest 2 (
          break 1 ^^
          group (it ^/^ equals ^/^ start) ^/^
          dir ^/^
          stop
        ) ^/^
        string "do"
      ) ^^
      nest 2 (break 1 ^^ body) ^/^
      string "done"
    )

  and pp_constraint exp ct =
    match exp.pexp_desc with
    | Pexp_pack me -> pp_pack me (Some ct)
    | _ ->
      let exp = pp [] exp in
      let ct = Core_type.pp [] ct in
      group (parens (exp ^/^ colon ^/^ ct))

  and pp_coerce exp ct_start ct =
    let exp = pp [] exp in
    let ct_start =
      optional (fun ct -> break 1 ^^ colon ^/^ Core_type.pp [] ct) ct_start
    in
    let ct = Core_type.pp [] ct in
    group (parens (group (exp ^^ ct_start) ^/^ !^":>" ^/^  ct))

  and pp_send ps exp met =
    let exp = pp ps exp in
    let met = string met.txt in
    let doc = flow (break 0) [ exp; sharp; met ] in
    Printing_stack.parenthesize ps doc

  and pp_new lid =
    Longident.pp lid.txt

  and pp_setinstvar ps lbl exp =
    let lbl = string lbl.txt in
    let exp = pp (List.tl ps) exp in
    (* FIXME: parens? *)
    lbl ^/^ larrow ^/^ exp

  and obj_field_override (lbl, exp) =
    let fld = string lbl.txt in
    let exp = pp [ Printing_stack.Record_field ] exp in
    fld ^/^ equals ^/^ exp

  and pp_override fields =
    let fields = separate_map (semi ^^ break 1) obj_field_override fields in
    (* FIXME: breaking, indent, blablabla *)
    braces (angles fields)

  and pp_letmodule ps name (params, typ, mexp) expr =
    let binding = Module_binding.pp_raw name params typ mexp in
    let bind = Binding.pp ~keyword:(group (let_ ^/^ module_)) binding in
    let expr =
      let ps = if Printing_stack.will_parenthesize ps then [] else List.tl ps in
      pp ps expr
    in
    let doc = bind ^/^ in_ ^/^ expr in
    Printing_stack.parenthesize ps doc

  and pp_letexception _exn _exp =
    assert false

  and pp_assert ps exp =
    let exp = pp ps exp in
    !^"assert" ^/^ exp

  and pp_lazy ps exp =
    let exp = pp ps exp in
    let doc = lazy_ ^/^ exp in
    Printing_stack.parenthesize ps doc

  and pp_object cl =
    let cl = Class_structure.pp cl in
    group (
      string "object" ^^
      nest 2 (break 1 ^^ cl) ^/^
      string "end"
    )


  and pp_pack me constr_opt =
    let me = Module_expr.pp me in
    let constraint_ =
      (* FIXME: factorize with pattern unpack *)
      match constr_opt with
      | None -> empty
      | Some ct ->
        match ct.ptyp_desc with
        | Ptyp_package pkg -> break 1 ^^ colon ^/^ Package_type.pp pkg
        | _ -> assert false
    in
    parens (module_ ^/^ me ^^ constraint_)

  and pp_open lid exp =
    let lid = Longident.pp lid.txt in
    let exp = pp [] exp in
    lid ^^ dot ^^ parens (break 0 ^^ exp ^^ break 0)

  and pp_letopen ps od exp =
    let od = Open_declaration.pp Attached_to_item od in
    let exp =
      let ps = if Printing_stack.will_parenthesize ps then [] else List.tl ps in
      pp ps exp
    in
    let doc = !^"let " ^^ od ^^ !^" in" ^/^ exp in
    Printing_stack.parenthesize ps doc

  and pp_letop _ =
    assert false
end

and Fun_param : sig
  val pp : fun_param -> document
end = struct
  let term lbl default pat =
    let suffix lbl =
      match pat.ppat_desc with
      | Ppat_var v when lbl = v.txt -> empty
      | _ -> colon ^^ Pattern.pp [ Printing_stack.Value_binding ] pat
    in
    match lbl with
    | Nolabel -> Pattern.pp [ Printing_stack.Value_binding ] pat
    | Labelled lbl -> tilde ^^ string lbl ^^ suffix lbl
    | Optional lbl ->
      match default with
      | None -> qmark ^^ string lbl ^^ suffix lbl
      | Some def ->
        (* TODO: punning *)
        let pat = Pattern.pp [] pat in
        let exp = Expression.pp [ Printing_stack.Value_binding ] def in
        qmark ^^ string lbl ^^ colon ^^ parens (group (pat ^/^ equals ^/^ exp))

  let newtype typ =
    parens (!^"type" ^/^ string typ.txt)

  let pp = function
    | Term (lbl, default, pat) -> term lbl default pat
    | Type typ -> newtype typ
end

and Value_binding : sig
  val pp : Attribute.kind -> value_binding -> document * document
end = struct

  let pp attr_kind
      { pvb_pat; pvb_params; pvb_type; pvb_expr; pvb_attributes; _ } =
    let lhs =
      let pat = Pattern.pp [ Printing_stack.Value_binding ] pvb_pat in
      let params =
        match pvb_params with
        | [] -> empty
        | lst -> break 1 ^^ left_assoc_map ~sep:empty ~f:Fun_param.pp lst
      in
      let typ =
        let ty1, ty2 = pvb_type in
        let pp ty = break 1 ^^ Core_type.pp [] ty in
        optional pp ty1 ^^ optional pp ty2
      in
      pat ^^ params ^^ typ
    in
    let rhs = Expression.pp [ Printing_stack.Value_binding ] pvb_expr in
    let rhs = Attribute.attach attr_kind rhs pvb_attributes in
    lhs, rhs
end

and Module_expr : sig
  val pp : module_expr -> document
end = struct
  let rec pp { pmod_desc; pmod_attributes; _ } =
    let doc = pp_desc pmod_desc in
    Attribute.attach_to_item doc pmod_attributes

  and pp_desc = function
    | Pmod_ident lid -> Longident.pp lid.txt
    | Pmod_structure str -> pp_structure str
    | Pmod_functor (Unit, me) -> pp_generative_functor me
    | Pmod_functor (Named (param, mty), me) ->
      pp_applicative_functor param mty me
    | Pmod_apply (me1, me2) -> pp_apply me1 me2
    | Pmod_constraint (me, mty) -> pp_constraint me mty
    | Pmod_unpack e -> pp_unpack e
    | Pmod_extension ext -> Extension.pp Item ext

  and pp_structure str =
    let str = Structure.pp str in
    group (
      string "struct" ^^
      nest 2 (break 1 ^^ str) ^/^
      string "end"
    )

  and pp_generative_functor me =
    let me = pp me in
    !^"functor" ^/^ !^"()" ^/^ arrow ^/^ me

  and pp_applicative_functor param mty me =
    let param = module_name param.txt in
    let mty = Module_type.pp mty in
    let me = pp me in
    !^"functor" ^/^ parens (param ^/^ colon ^/^ mty) ^/^ arrow ^/^ me

  and pp_apply me1 me2 =
    let me1 = pp me1 in
    let me2 = pp me2 in
    me1 ^^ break 0 ^^ parens me2

  and pp_constraint me mty =
    let me = pp me in
    let mty = Module_type.pp mty in
    parens (me ^/^ colon ^/^ mty)

  and pp_unpack exp =
    let exp = Expression.pp [ Unpack ] exp in
    parens (!^"val" ^^ exp)
end

and Module_type : sig
  val pp : module_type -> document
end = struct
  let rec pp { pmty_desc; pmty_attributes; _ } =
    Attribute.attach_to_item (pp_desc pmty_desc) pmty_attributes

  and pp_desc = function
    | Pmty_ident lid -> Longident.pp lid.txt
    | Pmty_signature sg -> pp_signature sg
    | Pmty_functor (Unit, mty) -> pp_generative_functor mty
    | Pmty_functor (Named (param, pmty), mty) ->
      pp_applicative_functor param pmty mty
    | Pmty_with (mty, cstrs) -> pp_with mty cstrs
    | Pmty_typeof me -> pp_typeof me
    | Pmty_extension ext -> Extension.pp Item ext
    | Pmty_alias _ -> assert false (* shouldn't be produced by the parser. *)

  and pp_signature sg =
    let sg = Signature.pp sg in
    group (
      string "sig" ^^
      nest 2 (break 1 ^^ sg) ^/^
      string "end"
    )

  and pp_generative_functor mty =
    let me = pp mty in
    !^"functor" ^/^ !^"()" ^/^ arrow ^/^ me

  and pp_applicative_functor param pmty mty =
    let param = module_name param.txt in
    let pmty = pp pmty in
    let mty = pp mty in
    !^"functor" ^/^ parens (param ^/^ colon ^/^ pmty) ^/^ arrow ^/^ mty

  (* TODO *)
  and pp_with mty _cstrs =
    let mty = pp mty in
    mty ^/^ !^"with <CSTRS>" 

  and pp_typeof exp =
    let me = Module_expr.pp exp in
    flow (break 1) [
      module_; !^"type"; of_; me
    ]

end

and Module_binding : sig
  val pp_raw
    :  string option loc
    -> functor_parameter list
    -> module_type option
    -> module_expr
    -> Binding.t

  val pp : module_binding -> Binding.t
end = struct
  let param = function
    | Unit -> !^"()"
    | Named (name, mty) ->
      group (
        parens (
          prefix ~indent:2 ~spaces:1
            (group (module_name name.txt ^/^ colon))
            (Module_type.pp mty)
        )
      )

  let pp_raw name params mty me =
    let lhs = module_name name.txt in
    let lhs =
      match params with
      | [] -> lhs
      | _ -> separate_map (break 1) param params
    in
    let typ = Option.map Module_type.pp mty in
    let rhs = Module_expr.pp me in
    { Binding. lhs; typ; rhs }

  (* TODO: proper printing *)
  let pp { pmb_name; pmb_params; pmb_type; pmb_expr; pmb_attributes; _ } =
    let binding = pp_raw pmb_name pmb_params pmb_type pmb_expr in
    let rhs = Attribute.attach_to_top_item binding.rhs pmb_attributes in
    { binding with rhs }
end

and Module_type_declaration : sig
  val pp : module_type_declaration -> document
end = struct
  let pp { pmtd_name; pmtd_type; pmtd_attributes; _ } =
    let kw = !^"module type" in
    let name = string pmtd_name.txt in
    let doc =
      match pmtd_type with
      | None -> kw ^/^ name
      | Some mty ->
        let typ = Module_type.pp mty in
        Binding.pp ~keyword:kw
          { lhs = name; rhs = typ; typ = None }
    in
    Attribute.attach_to_top_item doc pmtd_attributes
end

and Structure : sig
  val pp : structure -> document
end = struct
  let pp_eval exp attrs =
    let exp = Expression.pp [] exp in
    Attribute.attach_to_top_item exp attrs

  and rec_flag = function
    | Recursive -> string " rec"
    | Nonrecursive -> empty

  let pp_value rf vbs =
    let vbs =
      List.mapi (fun i vb ->
        let lhs, rhs = Value_binding.pp Attached_to_structure_item vb in
        let keyword = if i = 0 then let_ ^^ rec_flag rf else and_ in
        Binding.pp ~keyword
          { lhs; rhs; typ = None }
      ) vbs
    in
    separate (twice hardline) vbs

  let pp_module mb =
    Binding.pp ~keyword:module_ (Module_binding.pp mb)

  let pp_recmodule mbs =
    let mbs =
      List.mapi (fun i mb ->
        let keyword = if i = 0 then group (module_ ^/^ !^"rec") else and_ in
        Binding.pp ~keyword (Module_binding.pp mb)
      ) mbs
    in
    separate (twice hardline) mbs

  let pp_include { pincl_mod; pincl_attributes; _ } =
    let incl = Module_expr.pp pincl_mod in
    Attribute.attach_to_top_item 
      (group (!^"include" ^/^ incl))
      pincl_attributes

  let pp_extension ext attrs =
    let ext = Extension.pp Structure_item ext in
    Attribute.attach_to_top_item ext attrs

  let pp_item ({ pstr_desc; _ } as _item) =
    match pstr_desc with
    | Pstr_eval (e, attrs) -> pp_eval e attrs
    | Pstr_value (rf, vbs) -> pp_value rf vbs
    | Pstr_primitive vd -> Value_description.pp vd
    | Pstr_type (rf, tds) -> Type_declaration.pp_decl rf tds
    | Pstr_typext _ -> assert false
    | Pstr_exception _ -> assert false
    | Pstr_module mb -> pp_module mb
    | Pstr_recmodule mbs -> pp_recmodule mbs
    | Pstr_modtype mtd -> Module_type_declaration.pp mtd
    | Pstr_open od -> Open_declaration.pp Attached_to_structure_item od
    | Pstr_class _ -> assert false
    | Pstr_class_type _ -> assert false
    | Pstr_include incl -> pp_include incl
    | Pstr_attribute attr -> Attribute.pp Free_floating attr
    | Pstr_extension (ext, attrs) -> pp_extension ext attrs

  let pp = separate_map (twice hardline) pp_item
end

and Signature : sig
  val pp : signature -> document
end = struct
  let pp_extension ext attrs =
    let ext = Extension.pp Structure_item ext in
    Attribute.attach_to_top_item ext attrs

  let pp_include { pincl_mod; pincl_attributes; _ } =
    let incl = Module_type.pp pincl_mod in
    Attribute.attach_to_top_item 
      (group (!^"include" ^/^ incl))
      pincl_attributes

  let pp_item { psig_desc; _ } =
    match psig_desc with
    | Psig_value vd -> Value_description.pp vd
    | Psig_type (rf, decls) -> Type_declaration.pp_decl rf decls
    | Psig_typesubst decls -> Type_declaration.pp_subst decls
    | Psig_modtype mtd -> Module_type_declaration.pp mtd
    | Psig_open od -> Open_description.pp od
    | Psig_include incl -> pp_include incl
    | Psig_attribute attr -> Attribute.pp Free_floating attr
    | Psig_extension (ext, attrs) -> pp_extension ext attrs
    | _ -> assert false

  let pp = separate_map (twice hardline) pp_item
end

and Value_description : sig
  val pp : value_description -> document
end = struct
  let pp vd =
    let name = string vd.pval_name.txt in
    let ctyp = Core_type.pp [] vd.pval_type in
    let prim =
      match vd.pval_prim with
      | [] -> empty
      | prims ->
        let prims = separate_map (break 1) (fun p -> dquotes (string p)) prims in
        break 1 ^^ group (equals ^/^ prims)
    in
    let doc =
      prefix ~indent:2 ~spaces:1 (group (!^"val" ^/^ name))
        (colon ^^ ifflat space (twice space) ^^ ctyp ^^ prim)
    in
    Attribute.attach_to_top_item doc vd.pval_attributes
end

and Type_declaration : sig
  val pp : type_declaration -> document * document

  val pp_decl : rec_flag -> type_declaration list -> document

  val pp_subst : type_declaration list -> document
end = struct
  let pp_param (ct, var) =
    let ct = Core_type.pp [] ct in
    match var with
    | Invariant -> ct
    | Covariant -> plus ^^ ct
    | Contravariant -> minus ^^ ct

  let pp_params lst =
    match lst with
    | [] -> empty
    | [ x ] -> pp_param x ^^ break 1
    | _ -> parens (separate_map (comma ^^ break 1) pp_param lst) ^^ break 1

  let label_declaration { pld_name; pld_mutable; pld_type; pld_attributes; _ } =
    let mutable_ =
      match pld_mutable with
      | Mutable -> !^"mutable" ^^ break 1
      | Immutable -> empty
    in
    let name = string pld_name.txt in
    let typ  = Core_type.pp [] pld_type in
    let decl =
      group (
        nest 2 (
          group (mutable_ ^^ group (name ^/^ colon)) ^/^ typ
        )
      )
    in
    Attribute.attach_to_item decl pld_attributes

  let record lbl_decls =
    let lbls = separate_map (semi ^^ break 1) label_declaration lbl_decls in
    lbrace ^^
    nest 2 (break 1 ^^ lbls) ^/^
    rbrace

  let constructor_arguments = function
    | Pcstr_record lbl_decls -> record lbl_decls
    | Pcstr_tuple args ->
      let printing_stack =
        (* morally equivalent to: *)
        [ Printing_stack.Core_type (Ptyp_tuple args) ]
      in
      separate_map (break 1 ^^ star ^^ break 1)
        (Core_type.pp printing_stack) args

  let gadt_constructor { pcd_name; pcd_args; pcd_res; pcd_attributes; _ } =
    let name = string pcd_name.txt in
    let args = constructor_arguments pcd_args in
    let res  = Core_type.pp [] (Option.get pcd_res) in
    let decl = name ^/^ colon ^/^ args ^/^ arrow ^/^ res in
    Attribute.attach_to_item decl pcd_attributes

  let simple_constructor { pcd_name; pcd_args; pcd_attributes; _ } =
    let name = string pcd_name.txt in
    let args = constructor_arguments pcd_args in
    let decl =
      group (
        prefix ~indent:2 ~spaces:1
          (name ^/^ of_)
          args
      )
    in
    Attribute.attach_to_item decl pcd_attributes

  let constructor cstr =
    match cstr.pcd_res with
    | None -> simple_constructor cstr
    | Some _ -> gadt_constructor cstr

  let variant cstrs =
    let cstrs = separate_map (break 1 ^^ pipe ^^ space) constructor cstrs in
    let prefix = ifflat empty (pipe ^^ space) in
    prefix ^^ cstrs

  let type_kind = function
    | Ptype_abstract -> empty
    | Ptype_open -> dotdot
    | Ptype_record lbl_decls -> record lbl_decls
    | Ptype_variant cstrs -> variant cstrs 

  (* TODO: constraints *)
  let pp { ptype_name; ptype_params; ptype_cstrs = _; ptype_kind; ptype_private;
           ptype_manifest; ptype_attributes; _ } =
    let name = string ptype_name.txt in
    let params = pp_params ptype_params in
    let kind = type_kind ptype_kind in
    let manifest = optional (Core_type.pp []) ptype_manifest in
    let private_ =
      match ptype_private with
      | Private -> !^"private" ^^ break 1
      | Public -> empty
    in
    let lhs = group (params ^^ name) in
    let rhs =
      match ptype_manifest, ptype_kind with
      | Some _, Ptype_abstract -> private_ ^^ manifest
      | Some _, _ -> manifest ^/^ equals ^/^ private_ ^^ kind
      | None, _ -> private_ ^^ kind
    in
    let rhs = Attribute.attach_to_top_item rhs ptype_attributes in
    lhs, rhs

  let rec_flag = function
    | Recursive -> empty
    | Nonrecursive -> !^" nonrec "

  let pp_decl rf decls =
    let decls =
      List.mapi (fun i decl ->
        let lhs, rhs = pp decl in
        let keyword = if i = 0 then !^"type" ^^ rec_flag rf else and_ in
        Binding.pp ~keyword { lhs; rhs; typ = None }
      ) decls
    in
    separate (twice hardline) decls

  let pp_subst decls =
    let binder = !^":=" in
    let decls =
      List.mapi (fun i decl ->
        let lhs, rhs = pp decl in
        let keyword = if i = 0 then !^"type" else and_ in
        Binding.pp ~binder ~keyword { lhs; rhs; typ = None }
      ) decls
    in
    separate hardline decls
end

and Class_structure : sig
  val pp : class_structure -> document
end = struct
  let pp _ = assert false
end

and Open_description : sig
  val pp : open_description -> document
end = struct
  let pp { popen_expr; popen_override; popen_attributes; _ } =
    let expr = Longident.pp popen_expr.txt in
    let over =
      match popen_override with
      | Override -> bang
      | _ -> empty
    in
    let opn = group (!^"open" ^^ over ^/^ expr) in
    Attribute.attach_to_top_item opn popen_attributes
end

and Open_declaration : sig
  val pp : Attribute.kind -> open_declaration -> document
end = struct
  let pp kind { popen_expr; popen_override; popen_attributes; _ } =
    let expr = Module_expr.pp popen_expr in
    let over =
      match popen_override with
      | Override -> bang
      | _ -> empty
    in
    let opn = group (!^"open" ^^ over ^/^ expr) in
    Attribute.attach kind opn popen_attributes
end
