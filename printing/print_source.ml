open Document
open Custom_combinators
open Import
open Asttypes
open Source_tree

let rec list_last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> list_last xs

let under_app =
  List.exists (function
    | Printing_stack.Expression Pexp_apply _ -> true
    | _ -> false
  )

let module_name { txt; loc } =
  match txt with
  | None -> underscore ~loc
  | Some name -> string ~loc name

let rec_token ~recursive_by_default rf : Source_parsing.Parser.token option =
  match rf, recursive_by_default with
  | Recursive, false -> Some REC
  | Nonrecursive, true -> Some NONREC
  | _, _ -> None

module Constant : sig
  val pp : loc:Location.t -> constant -> document
  (* Helpers. *)
  val pp_string_lit : loc:Location.t -> string -> document
(*   val pp_quoted_string : loc:Location.t -> delim:string -> string -> document
     *)
end = struct
  let pp_string_lit ~loc _s =
    let s =
      Source_parsing.Source.source_between loc.Location.loc_start loc.loc_end
    in
    quoted_string ~loc s

  (*
  let pp_quoted_string ~loc ~delim s =
    let delim = PPrint.string delim in
    braces
      (enclose ~before:PPrint.(delim ^^ bar) ~after:PPrint.(bar ^^ delim)
        (quoted_string ~loc s))
     *)

  let pp ~loc = function
    | Pconst_float (nb, suffix_opt) | Pconst_integer (nb, suffix_opt) ->
      let nb =
        match suffix_opt with
        | None -> nb
        | Some s -> nb ^ (String.make 1 s)
      in
      string ~loc nb
    | Pconst_char c -> char ~loc c
    | Pconst_string (_, None) ->
      let s = Source_parsing.Source.source_between loc.loc_start loc.loc_end in
      quoted_string ~adjust_indent:true ~loc s
    | Pconst_string (_, Some _) ->
      let s = Source_parsing.Source.source_between loc.loc_start loc.loc_end in
      quoted_string ~loc s
end


module Polymorphic_variant_tag : sig
  val pp : label loc -> document
end = struct
  let pp tag =
    string ~loc:tag.loc ("`" ^ tag.txt)
end


module rec Empty_delimited : sig
  val pp
    :  loc:Location.t
    -> ?extension:string loc
    -> attributes
    -> Parser.token
    -> Parser.token
    -> document
end = struct
  let pp ~(loc:Location.t) ?extension attrs start_tok end_tok =
    let fst =
      let token = Token.pp ~inside:loc start_tok in
      Keyword.decorate token ~extension attrs
    in
    let snd = Token.pp ~inside:loc end_tok in
    group (fst ^/^ snd)
end


and Pattern : sig
  val pp : ?indent:int -> pattern -> document
end = struct
  let rec pp
      ?indent { ppat_desc; ppat_attributes; ppat_loc; ppat_ext_attributes; _ }
  =
    let desc =
      pp_desc ?indent ~loc:ppat_loc ~ext_attrs:ppat_ext_attributes ppat_desc
    in
    Attribute.attach_to_item desc ppat_attributes

  and pp_alias pat alias =
    let pat = pp pat in
    let alias = str alias in
    let as_ = Token.pp ~after:pat ~before:alias AS in
    let aliaser = group (as_ ^/^ alias) in
    let doc = pat ^^ group (nest 2 (break 1 ^^ aliaser)) in
    doc

  and pp_interval c1 c2 =
    let c1 = Constant.pp ~loc:c1.loc c1.txt in
    let c2 = Constant.pp ~loc:c2.loc c2.txt in
    let dotdot = Token.pp ~after:c1 ~before:c2 DOTDOT in
    c1 ^/^ dotdot ^/^ c2

  (* FIXME? nest on the outside, not in each of them. *)
  and pp_tuple lst =
    group (tuple_fields pp (List.hd lst) (List.tl lst))

  and pp_list_literal ~loc elts =
    List_like.pp ~loc ~formatting:Wrap (* TODO: add an option *) ~left:LBRACKET
      ~right:RBRACKET (List.map pp elts)

  and pp_cons hd tl =
    let hd = pp hd in
    let tl = pp tl in
    let cons = Token.pp ~after:hd ~before:tl COLONCOLON in
    let doc = infix ~indent:2 ~spaces:1 cons hd tl in
    doc

  and pp_construct name arg_opt =
    let name = Longident.pp name in
    match arg_opt with
    | None -> name
    | Some (vars, p) ->
      let p = pp p in
      let p =
        match vars with
        | [] -> p
        | v :: vs ->
          let vars = flow (break 1) (str v) (List.map str vs) in
          let lpar = Token.pp ~after:name ~before:vars LPAREN in
          let typ = Token.pp ~after:lpar ~before:vars TYPE in
          let rpar = Token.pp ~after:vars ~before:p TYPE in
          prefix ~indent:0 ~spaces:1 (group (lpar ^^ typ ^/^ vars ^^ rpar)) p
      in
      prefix ~indent:2 ~spaces:1 name p

  and pp_variant tag arg_opt =
    let tag = Polymorphic_variant_tag.pp tag in
    match arg_opt with
    | None -> tag
    | Some p ->
      let arg = pp p in
      (tag ^/^ arg)

  and pp_record_field (lid, ctyo, pato) =
    let binding : Binding.t =
      {
        lhs = Longident.pp lid;
        params = [];
        constr = Option.map Core_type.pp ctyo;
        coerce = None;
        rhs = Binding.Rhs.of_opt pp pato
      }
    in
    group (Binding.pp binding)

  and pp_record ~loc pats closed =
    let fields = List.map pp_record_field pats in
    let fields =
      match closed with
      | OClosed -> fields
      | OOpen loc -> fields @ [ underscore ~loc ]
    in
    List_like.pp ~loc ~formatting:!Options.Record.pattern ~left:LBRACE
      ~right:RBRACE fields

  and pp_array ~loc pats =
    let pats = List.map pp pats in
    (* TODO: add an option *)
    List_like.pp ~loc ~formatting:Wrap ~left:LBRACKETBAR ~right:BARRBRACKET pats

  and pp_or ~indent p1 p2 =
    let p1 = pp ~indent p1 in
    let p2 = pp ~indent p2 in
    let pipe = Token.pp ~after:p1 ~before:p2 BAR in
    let or_ = p1 ^/^ group (pipe ^/^ p2) in
    or_

  and pp_constraint ~loc p ct =
    let p = pp p in
    let ct = Core_type.pp ct in
    let colon = Token.pp ~after:p ~before:ct COLON in
    parens ~loc (p ^/^ colon ^/^ ct)

  and pp_type ~loc typ =
    let typ = Longident.pp typ in
    let hash = Token.pp ~inside:loc ~before:typ HASH in
    hash ^^ typ

  and pp_lazy ~loc ~ext_attrs:(extension, attrs) p =
    let p = pp p in
    let kw =
      let tok = Token.pp ~inside:loc ~before:p LAZY in
      Keyword.decorate tok ~extension attrs
    in
    kw ^/^ p

  and pp_unpack ~loc ~ext_attrs:(extension, attrs) mod_name ct =
    let mod_name = module_name mod_name in
    let with_constraint =
      match ct with
      | None -> mod_name
      | Some pkg ->
        let constr = Core_type.Package_type.pp pkg in
        let colon = Token.pp ~after:mod_name ~before:constr COLON in
        mod_name ^/^ colon ^/^ constr
    in
    let module_ =
      let lpar = Token.pp ~inside:loc ~before:with_constraint LPAREN in
      let mod_ = Token.pp ~after:lpar ~before:with_constraint MODULE in
      Keyword.decorate (lpar ^^ mod_) ~extension attrs
    in
    let rparen = Token.pp ~inside:loc ~after:with_constraint RPAREN in
    prefix ~indent:2 ~spaces:1 module_ with_constraint ^/^ rparen

  and pp_exception ~loc ~ext_attrs:(extension, attrs) p =
    let p = pp p in
    let kw =
      let tok = Token.pp ~inside:loc ~before:p EXCEPTION in
      Keyword.decorate tok ~extension attrs
    in
    kw ^/^ p

  and pp_open lid p =
    let lid = Longident.pp lid in
    let pat = pp p in
    let dot = Token.pp ~after:lid ~before:pat DOT in
    lid ^^ dot ^^ break 0 ^^ pat

  and pp_var v =
    Longident.pp_ident v

  and pp_desc ?(indent=0) ~loc ~ext_attrs = function
    | Ppat_or (p1, p2) -> pp_or ~indent p1 p2
    | otherwise ->
      nest indent @@
        group
          (match otherwise with
          | Ppat_or _ -> assert false
          | Ppat_any -> underscore ~loc
          | Ppat_var v -> pp_var v
          | Ppat_parens p -> parens ~loc (pp p)
          | Ppat_alias (pat, alias) -> pp_alias pat alias
          | Ppat_constant c -> Constant.pp ~loc c
          | Ppat_interval (c1, c2) -> pp_interval c1 c2
          | Ppat_tuple pats -> pp_tuple pats
          | Ppat_construct (name, arg) -> pp_construct name arg
          | Ppat_list_lit pats -> pp_list_literal ~loc pats
          | Ppat_cons (hd, tl) -> pp_cons hd tl
          | Ppat_variant (tag, arg) -> pp_variant tag arg
          | Ppat_record (pats, closed) -> pp_record ~loc pats closed
          | Ppat_array pats -> pp_array ~loc pats
          | Ppat_constraint (p, ct) -> pp_constraint ~loc p ct
          | Ppat_type pt -> pp_type ~loc pt
          | Ppat_lazy p -> pp_lazy ~loc ~ext_attrs p
          | Ppat_unpack (name, typ) -> pp_unpack ~loc ~ext_attrs name typ
          | Ppat_exception p -> pp_exception ~loc ~ext_attrs p
          | Ppat_extension ext -> Attribute.Extension.pp ~loc Item ext
          | Ppat_open (lid, p) -> pp_open lid p)

  let () =
    Attribute.Payload.pp_pattern := pp
end


and Application : sig
  val pp_simple
    :  document
    -> (arg_label * expression)
    -> (arg_label * expression) list
    -> document

  val pp : expression -> (arg_label * expression) list -> document
  val pp_infix : string loc -> expression -> expression -> document
  val pp_prefix : string loc -> expression -> document
end = struct

  let arg_label ~later = function
      | Nolabel -> empty
      | Labelled { name; extra_info = `Single_token }
      | Optional { name; extra_info = `Single_token } -> str name
      | Labelled { name; extra_info = `Previous_token loc } ->
        let tilde = string ~loc "~" in
        let name = str name in
        let colon = Token.pp ~after:name ~before:later COLON in
        group (tilde ^^ name ^^ colon)
      | Optional { name; extra_info = `Previous_token loc } ->
        let tilde = string ~loc "?" in
        let name = str name in
        let colon = Token.pp ~after:name ~before:later COLON in
        group (tilde ^^ name ^^ colon)

  let perhaps_pun id = function
    | Labelled { name; extra_info = `Previous_token loc }
      when name.txt = id.txt ->
      let tilde = string ~loc "~" in
      tilde ^^ str name
    | Optional { name; extra_info = `Previous_token loc }
      when name.txt = id.txt ->
      let question = string ~loc "?" in
      question ^^ str name
    | lbl ->
      let id = Longident.pp_ident id in
      let lbl = arg_label ~later:id lbl in
      lbl ^^ id

  (* Only called when a literal function is inside the parentheses
     So we don't have to handle cases like ~(foo:x) which are [Pexp_constraint]
  *)
  let get_delims ~ext_attrs:(extension, attrs) ~begin_end ~loc lbl kw body =
    let opening =
      if begin_end then (
        let begin_ = Token.pp ~inside:loc ~before:kw BEGIN in
        Keyword.decorate begin_ ~extension attrs ^^ break 1
      ) else (
        (* Can't put [%foo[@bar]] on '(' *)
        Token.pp ~inside:loc ~before:kw LPAREN ^^ break 0
      )
    in
    let lbl = arg_label ~later:opening lbl in
    let closing =
      if begin_end then
        break 1 ^^ Token.pp ~after:body ~inside:loc END
      else
        break 0 ^^ Token.pp ~after:body ~inside:loc RPAREN
    in
    lbl, opening, closing

  let argument (lbl, exp) =
    match exp with
    | { pexp_desc = Pexp_ident Lident id; _ } -> perhaps_pun id lbl
    | { pexp_desc =
          Pexp_parens
            { exp = { pexp_desc = Pexp_fun (params, tycstr, body); _ } as exp;
              begin_end };
        pexp_attributes = []; pexp_loc = loc;
        pexp_ext_attributes = ext_attrs;
        _ }
      ->
      let fun_, args, arrow, body =
        Expression.fun_chunks ~loc:exp.pexp_loc
          ~ext_attrs:exp.pexp_ext_attributes params tycstr body
      in
      let lbl, opening, closing =
        get_delims ~ext_attrs ~begin_end ~loc lbl fun_ body
      in
      let open_ = lbl ^^ opening ^^ fun_ in
      let unclosed =
        prefix ~indent:2 ~spaces:1
          (group ((prefix ~indent:2 ~spaces:1 open_ args) ^/^ arrow)) body
      in
      unclosed ^^ closing
    | { pexp_desc =
          Pexp_parens
            { exp = { pexp_desc = Pexp_function (c :: cs); _ } as exp;
              begin_end };
        pexp_attributes = []; pexp_loc = loc;
        pexp_ext_attributes = ext_attrs;
        _ }
      ->
      let compact =
        match !Options.Match.compact with
        | Multi -> false
        | _ -> true
      in
      let function_, cases =
        Expression.function_chunks ~compact ~loc:exp.pexp_loc
          ~ext_attrs:exp.pexp_ext_attributes c cs
      in
      let lbl, opening, closing =
        get_delims ~ext_attrs ~begin_end ~loc lbl function_ cases
      in
      let open_ = lbl ^^ opening ^^ function_ in
      open_ ^^ cases ^^ closing
    | _ ->
      let exp = Expression.pp exp in
      let lbl = arg_label ~later:exp lbl in
      group (lbl ^^ break 0 ^^ exp)

  type argument =
    | Function of
        {
          fst_chunk : document;
          break : bool;
          snd_chunk : document;
          closing : document
        }
    | Fully_built of document

  let rec combine_app_chunks acc = function
    | [] -> acc
    | Function { fst_chunk; break = b; snd_chunk; closing } :: rest ->
      let d1 = break 1 ^^ fst_chunk in
      let d2 = if b then break 1 ^^ snd_chunk else snd_chunk in
      let fn = (nest 2 @@ (ifflat d1 (group d1)) ^^ d2) ^^ closing in
      combine_app_chunks (acc ^^ group fn) rest
    | Fully_built doc :: rest ->
      combine_app_chunks (acc ^^ nest 2 @@ group (break 1 ^^ doc)) rest

  let smart_arg (lbl, exp) =
    match exp with
    | { pexp_desc =
          Pexp_parens
            { exp =
                { pexp_desc = Pexp_fun (params, ty, body);
                  pexp_attributes = [];
                  _ } as exp;
              begin_end };
        pexp_attributes = []; pexp_loc = loc; pexp_ext_attributes = ext_attrs; _
      }
      ->
      let fun_, args, arrow, body =
        Expression.fun_chunks ~loc:exp.pexp_loc
          ~ext_attrs:exp.pexp_ext_attributes params ty body
      in
      let lbl, opening, closing =
        get_delims ~ext_attrs ~begin_end ~loc lbl fun_ body
      in
      let first_chunk =
        group ((prefix ~indent:2 ~spaces:1 fun_ args) ^/^ arrow)
      in
      let fst_chunk = lbl ^^ group (opening ^^ first_chunk) in
      let snd_chunk = group body in
      Function { fst_chunk; snd_chunk; break = true; closing }
    | { pexp_desc =
          Pexp_parens
            { exp =
                { pexp_desc = Pexp_function (c :: cs); pexp_attributes = []; _ }
                  as exp;
              begin_end };
        pexp_attributes = []; pexp_loc = loc; pexp_ext_attributes = ext_attrs; _
      }
      ->
      let compact =
        match !Options.Match.compact with
        | Multi -> false
        | _ -> true
      in
      let function_, cases =
        Expression.function_chunks ~compact ~loc:exp.pexp_loc
          ~ext_attrs:exp.pexp_ext_attributes c cs
      in
      let lbl, opening, closing =
        get_delims ~ext_attrs ~begin_end ~loc lbl function_ cases
      in
      let fst_chunk = lbl ^^ group (opening ^^ function_) in
      let snd_chunk = group cases in
      Function { fst_chunk; snd_chunk; break = false; closing }
    | arg -> Fully_built (argument (lbl, arg))

  let pp_simple applied arg args =
    let fit_or_vertical () =
      let args = separate_map (break 1) ~f:argument arg args in
      prefix ~indent:2 ~spaces:1 applied args
    in
    let doc =
      match !Options.Applications.layout with
      | Fit_or_vertical -> fit_or_vertical ()
      | Wrap ->
        let args = List.map argument (arg :: args) in
        nest 2 @@ left_assoc_map ~f:Fun.id applied args
      | Smart ->
        let nb_labels, len_labels =
          List.fold_left (fun (nb, len) (lbl, _) ->
            match lbl with
            | Nolabel -> nb, len
            | Labelled lbl | Optional lbl ->
              nb + 1, len + String.length lbl.name.txt
          ) (0, 0) args
        in
        (* It would be nice if I could have "current indent + len_labels" ...
           maybe. *)
        (* HACKISH *)
        if nb_labels > 4 && len_labels > 8 then
          fit_or_vertical ()
        else
          let args = List.map smart_arg (arg :: args) in
          combine_app_chunks applied args
    in
    doc

  let simple_apply exp arg args =
    let exp = Expression.pp exp in
    pp_simple exp arg args

  let _classify_fun exp =
    match exp.pexp_desc with
    | Pexp_ident Lident s when s.txt <> "" -> Ident_class.classify s
    | _ -> Normal

  let pp_prefix op arg =
    let sep =
      (* FIXME: this is most likely incomplete. *)
      match arg.pexp_desc with
      | Pexp_prefix_apply _
      | Pexp_field ({ pexp_desc = Pexp_prefix_apply _; _ }, _)
        ->
        break 1
      | _ -> empty
    in
    let op = str op in
    let arg = Expression.pp arg in
    nest 2 (op ^^ sep ^^ arg)

  let pp exp = function
    | [] ->
      (* An application node without arguments? That can't happen. *)
      assert false
    | arg :: args -> simple_apply exp arg args

  let pp_infix op arg1 arg2 =
    let fst = Expression.pp arg1 in
    let snd = Expression.pp arg2 in
    infix ~indent:2 ~spaces:1 (str op) fst snd
end


and Expression : sig
  val pp : expression -> document

  val function_chunks
    :  compact:bool
    -> loc:Location.t
    -> ext_attrs:string loc option * attributes
    -> case
    -> case list
    -> document * document

  val fun_chunks
    :  loc:Location.t
    -> ext_attrs:string loc option * attributes
    -> fun_param list
    -> core_type option
    -> expression
    -> document * document * document * document
end = struct
  let rec pp { pexp_desc; pexp_attributes; pexp_ext_attributes; pexp_loc; _ } =
    let desc =
      group (pp_desc ~ext_attrs:pexp_ext_attributes ~loc:pexp_loc pexp_desc)
    in
    Attribute.attach_to_item desc pexp_attributes

  and pp_desc ~loc ~ext_attrs = function
    | Pexp_parens { exp; begin_end } -> pp_parens ~loc ~ext_attrs ~begin_end exp
    | Pexp_ident id -> pp_ident id
    | Pexp_constant c -> Constant.pp ~loc c
    | Pexp_let (rf, vbs, body) -> pp_let ~loc ~ext_attrs rf vbs body
    | Pexp_function cases -> pp_function ~loc ~ext_attrs cases
    | Pexp_fun (params, ty, exp) -> pp_fun ~loc ~ext_attrs params ty exp
    | Pexp_apply (expr, args) -> Application.pp expr args
    | Pexp_infix_apply (op, (arg1, arg2)) -> Application.pp_infix op arg1 arg2
    | Pexp_prefix_apply (op, arg) -> Application.pp_prefix op arg
    | Pexp_match (arg, cases) -> pp_match ~loc ~ext_attrs arg cases
    | Pexp_try (arg, cases) -> pp_try ~loc ~ext_attrs arg cases
    | Pexp_tuple exps -> pp_tuple exps
    | Pexp_list_lit exps -> pp_list_literal ~loc exps
    | Pexp_cons (hd, tl) -> pp_cons hd tl
    | Pexp_construct (lid, arg) -> pp_construct lid arg
    | Pexp_variant (tag, arg) -> pp_variant tag arg
    | Pexp_record (fields, exp) -> pp_record ~loc fields exp
    | Pexp_field (exp, fld) -> pp_field exp fld
    | Pexp_setfield (exp, fld, val_) -> pp_setfield exp fld val_
    | Pexp_array elts -> pp_array ~loc elts
    | Pexp_ifthen branches -> pp_if_then branches
    | Pexp_ifthenelse (branches, else_) -> pp_if_then_else branches else_
    | Pexp_sequence (e1, e2) -> pp_sequence ~ext_attrs e1 e2
    | Pexp_while (cond, body) -> pp_while ~loc ~ext_attrs cond body
    | Pexp_for (it, start, stop, dir, body) ->
      pp_for ~loc ~ext_attrs it start stop dir body
    | Pexp_constraint (e, ct) -> pp_constraint ~loc e ct
    | Pexp_coerce (e, ct_start, ct) -> pp_coerce ~loc e ct_start ct
    | Pexp_send (e, meth) -> pp_send e meth
    | Pexp_new lid -> pp_new ~loc ~ext_attrs lid
    | Pexp_setinstvar (lbl, exp) -> pp_setinstvar lbl exp
    | Pexp_override fields -> pp_override ~loc fields
    | Pexp_letmodule (name, mb, body) ->
      pp_letmodule ~loc ~ext_attrs name mb body
    | Pexp_letexception (exn, exp) -> pp_letexception ~loc ~ext_attrs exn exp
    | Pexp_assert exp -> pp_assert ~loc ~ext_attrs exp
    | Pexp_lazy exp -> pp_lazy ~loc ~ext_attrs exp
    | Pexp_object cl -> pp_object ~loc ~ext_attrs cl
    | Pexp_pack (me, pkg) -> pp_pack ~loc ~ext_attrs me pkg
    | Pexp_open (lid, exp) -> pp_open ~loc lid exp
    | Pexp_letopen (od, exp) -> pp_letopen ~loc ~ext_attrs od exp
    | Pexp_letop letop -> pp_letop letop
    | Pexp_extension ext -> Attribute.Extension.pp ~loc Item ext
    | Pexp_unreachable -> string ~loc "."
    | Pexp_access { accessed; paren; indices; set_expr } ->
      pp_access ~loc accessed paren indices set_expr
    | Pexp_dotop_access { accessed; path; op; paren; indices; set_expr } ->
      pp_dotop_access ~loc accessed path op paren indices set_expr

  and pp_ident =
    Longident.pp

  and pp_parens ~loc ~ext_attrs:(extension, attrs) ~begin_end e =
    if not begin_end then (
      assert (extension = None && attrs = []);
      parens ~loc (pp e)
    ) else (
      let end_ =
        Token.pp ~inside:{ loc with loc_start = e.pexp_loc.loc_end } END
      in
      let begin_ =
        let tok =
          Token.pp ~inside:{ loc with loc_end = e.pexp_loc.loc_start } BEGIN
        in
        Keyword.decorate tok ~extension attrs
      in
      match e.pexp_desc with
      | Pexp_match (arg, cases) when e.pexp_attributes = [] ->
        group
          (pp_match ~parens:(begin_, end_) ~loc:e.pexp_loc
            ~ext_attrs:e.pexp_ext_attributes arg cases)
      | _ -> (prefix ~indent:2 ~spaces:1 begin_ (pp e)) ^/^ end_
    )

  and pp_let ~ext_attrs:(extension, attrs) ~loc rf vbs body =
    assert (attrs = []);
    let vbs =
      let previous_vb = ref None in
      List.concat_map (fun vb ->
        let text, vb =
          let text, attrs =
            Attribute.extract_text vb.pvb_attributes
              ~item_start_pos:vb.pvb_loc.loc_start
          in
          text, { vb with  pvb_attributes = attrs }
        in
        let binding = Value_binding.pp Attached_to_structure_item vb in
        let keyword =
          let lhs = binding.lhs in
          let attrs =
            match vb.pvb_ext_attributes with
            | Some _, _ -> assert false
            | None, attrs -> attrs
          in
          let token, extension, modifier =
            match !previous_vb with
            | None ->
              Token.pp ~inside:loc ~before:lhs LET,
              extension,
              rec_token ~recursive_by_default:false rf
            | Some prev_vb ->
              Token.pp ~after:prev_vb ~before:lhs AND, None, None
          in
          let kw = Keyword.decorate token ~extension attrs in
          match modifier with
          | None -> kw
          | Some tok ->
            let modif = Token.pp ~after:kw ~before:lhs tok in
            kw ^/^ modif
        in
        let binding = Binding.pp ~keyword binding in
        previous_vb := Some binding;
        Attribute.prepend_text text binding
      ) vbs
    in
    let vbs = separate hardline (List.hd vbs) (List.tl vbs) in
    let body = pp body in
    let in_ = Token.pp ~after:vbs ~before:body IN in
    group (vbs ^/^ in_) ^//^ body

  and case_chunks { pc_lhs; pc_guard; pc_rhs } =
    let lhs = Pattern.pp ~indent:2 pc_lhs in
    let rhs = pp pc_rhs in
    let lhs =
      match pc_guard with
      | None ->
        let arrow = Token.pp ~after:lhs ~before:rhs MINUSGREATER in
        prefix ~indent:2 ~spaces:1 lhs arrow
      | Some guard ->
        let guarded =
          let guard = pp guard in
          let when_ = Token.pp ~after:lhs ~before:guard WHEN in
          prefix ~indent:2 ~spaces:1 when_ guard
        in
        let with_arrow =
          let arrow = Token.pp ~after:guarded ~before:rhs MINUSGREATER in
          group (guarded ^/^ arrow)
        in
        prefix ~spaces:1 ~indent:2 lhs with_arrow
    in
    lhs, rhs

  and case case =
    let lhs, rhs = case_chunks case in
    match !Options.Cases.body_on_separate_line with
    | Always -> lhs ^^ nest !Options.Cases.body_indent (hardline ^^ rhs)
    | When_needed -> prefix ~indent:!Options.Cases.body_indent ~spaces:1 lhs rhs

  and cases ~compact:compact_layout c cs =
    let fmt acc elt =
      let elt = case elt in
      let bar = Token.pp ~after:acc ~before:elt BAR in
      acc ^/^ group (bar ^^ space ^^ elt)
    in
    let rec iterator acc = function
      | [] -> acc
      | [ x ] -> fmt acc x
      | x :: xs -> iterator (fmt acc x) xs
    in
    let cases = iterator (case c) cs in
    (* FIXME: reuse bar from source if present *)
    let prefix =
      let open PPrint in
      let multi = hardline ^^ bar in
      (if compact_layout then ifflat empty multi else multi) ^^ space
    in
    prefix ++ cases

  and function_chunks ~compact ~loc ~ext_attrs:(extension, attrs) c cs =
    let cases = cases ~compact c cs in
    let keyword =
      let kw = Token.pp ~inside:loc ~before:cases FUNCTION in
      Keyword.decorate kw ~extension attrs
    in
    keyword, cases

  and pp_function ~loc ~ext_attrs = function
    | [] -> assert false (* always at least one case *)
    | c :: cs ->
      let compact =
        match !Options.Match.compact with
        | Multi -> false
        | Compact -> true
        | Compact_under_app -> false (* FIXME *)
      (* under_app ps *)
      in
      let keyword, cases = function_chunks ~compact ~loc ~ext_attrs c cs in
      (keyword ^^ cases)

  and fun_syntactic_elts ~loc ~ext_attrs:(extension, attrs) ~lhs ~rhs =
    let kw =
      let kw = Token.pp ~inside:loc ~before:lhs FUN in
      Keyword.decorate kw ~extension attrs
    in
    let arrow = Token.pp ~after:lhs ~before:rhs MINUSGREATER in
    kw, arrow

  and fun_chunks ~loc ~ext_attrs params tycstr exp =
    match params with
    | [] -> assert false
    | param :: params ->
      let args = left_assoc_map ~f:Fun_param.pp param params in
      let body = pp exp in
      let with_annot =
        match tycstr with
        | None -> args
        | Some cty ->
          let ty = Core_type.pp cty in
          let colon = Token.pp ~after:args ~before:ty COLON in
          args ^/^ group (colon ^/^ ty)
      in
      let kw, arrow =
        fun_syntactic_elts ~loc ~ext_attrs ~lhs:with_annot ~rhs:body
      in
      kw, with_annot, arrow, body

  and pp_fun ~loc ~ext_attrs params ty exp =
    let fun_, args, arrow, body = fun_chunks ~loc ~ext_attrs params ty exp in
    let doc =
      prefix ~indent:2 ~spaces:1
        (group ((prefix ~indent:2 ~spaces:1 fun_ args) ^/^ arrow)) body
    in
    doc

  and pp_match ?parens ~loc ~ext_attrs:(extension, attrs) arg = function
    | [] -> assert false (* always at least one case *)
    | c :: cs ->
      let arg = pp arg in
      let compact =
        match !Options.Match.compact with
        | Multi -> false
        | Compact -> true
        | Compact_under_app -> false (* FIXME: under_app ps *)
      in
      let cases = cases ~compact c cs in
      let match_ =
        let token = Token.pp ~inside:loc ~before:arg MATCH in
        Keyword.decorate token ~extension attrs
      in
      let with_ = Token.pp ~after:arg ~before:cases WITH in
      match parens with
      | None -> group (match_ ^^ nest 2 (break 1 ^^ arg) ^/^ with_) ^^ cases
      | Some (before, after) ->
        group (group (before ^/^ match_) ^^ nest 2 (break 1 ^^ arg) ^/^ with_)
        ^^ cases ^/^ after

  and pp_try ~loc ~ext_attrs:(extension, attrs) arg cs =
    let arg = pp arg in
    let try_ =
      let token = Token.pp ~inside:loc ~before:arg TRY in
      Keyword.decorate token ~extension attrs
    in
    let pprefix = prefix ~indent:2 ~spaces:1 in
    match cs with
    | [] -> assert false
    | [ c ] (* TODO: guard this layout under an option *) ->
      let lhs, rhs = case_chunks c in
      let with_ = Token.pp ~after:arg ~before:lhs WITH in
      pprefix try_ arg ^/^
        pprefix with_ lhs ^^ nest 2 ((ifflat (break 1) hardline) ^^ rhs)
    | c :: cs ->
      let compact =
        match !Options.Match.compact with
        | Multi -> false
        | Compact -> true
        | Compact_under_app -> false (* FIXME: under_app ps *)
      in
      let cases = cases ~compact c cs in
      let with_ = Token.pp ~after:arg ~before:cases WITH in
      pprefix try_ arg ^/^ with_ ^^ cases

  and pp_tuple = function
    | [] -> assert false
    | exp :: exps -> group (tuple_fields pp exp exps)

  and pp_construct lid arg_opt =
    let name = Longident.pp lid in
    match arg_opt with
    | None -> name
    | Some arg ->
      let arg = pp arg in
      let doc = prefix ~indent:2 ~spaces:1 name arg in
      doc

  and pp_cons hd tl =
    let hd = pp hd in
    let tl = pp tl in
    let cons = Token.pp ~after:hd ~before:tl COLONCOLON in
    let doc = infix ~indent:2 ~spaces:1 cons hd tl in
    doc

  and pp_list_literal ~loc elts =
    let elts = List.map pp elts in
    List_like.pp ~loc ~formatting:Wrap (* TODO: add an option *) ~left:LBRACKET
      ~right:RBRACKET elts

  and pp_variant tag arg_opt =
    let tag = Polymorphic_variant_tag.pp tag in
    match arg_opt with
    | None -> tag
    | Some arg ->
      let arg = pp arg in
      let doc = prefix ~indent:2 ~spaces:1 tag arg in
      doc

  and record_field (lid, (oct1, oct2), exp) =
    let binding : Binding.t =
      {
        lhs = Longident.pp lid;
        params = [];
        constr = Option.map Core_type.pp oct1;
        coerce = Option.map Core_type.pp oct2;
        rhs = Binding.Rhs.of_opt pp exp
      }
    in
    Binding.pp binding

  and pp_record ~loc fields updated_record =
    let fields = List.map record_field fields in
    match updated_record with
    | None ->
      List_like.pp ~loc ~formatting:!Options.Record.expression ~left:LBRACE
        ~right:RBRACE fields
    | Some e ->
      let update = pp e in
      let fields =
        List_like.pp_fields ~formatting:!Options.Record.expression
          (List.hd fields) (List.tl fields)
      in
      let with_ = Token.pp ~after:update ~before:fields WITH in
      braces ~loc
        ((group (group (break 1 ^^ update) ^/^ with_) ^/^ fields) ^^ break 1)

  and pp_field re fld =
    let record = pp re in
    let field = Longident.pp fld in
    let dot = Token.pp ~after:record ~before:field DOT in
    flow (break 0) record [ dot; field ]

  and pp_setfield re fld val_ =
    let field = pp_field re fld in
    let value = pp val_ in
    let larrow = Token.pp ~after:field ~before:value LESSMINUS in
    let doc = prefix ~indent:2 ~spaces:1 (group (field ^/^ larrow)) value in
    doc

  and pp_array ~loc elts =
    let elts = List.map pp elts in
    (* TODO: add an option *)
    List_like.pp ~loc ~formatting:Wrap ~left:LBRACKETBAR ~right:BARRBRACKET elts

  and pp_gen_access ~loc ?path ?dot paren_kind arr idx val_ =
    let arr = pp arr in
    let dot =
      match path, dot with
      | None, None -> Token.pp ~after:arr ~before:idx DOT
      | Some path, Some op ->
        let fstdot = Token.pp ~after:arr ~before:path DOT in
        group (fstdot ^^ path ^^ break 0 ^^ op)
      | None, Some op -> op
      | Some _, None -> assert false
    in
    let (left_tok, right_tok) : (Source_parsing.Parser.token as 't) * 't =
      match paren_kind with
      | Paren -> LPAREN, RPAREN
      | Brace -> LBRACE, RBRACE
      | Bracket -> LBRACKET, RBRACKET
    in
    let left = Token.pp ~after:dot ~before:idx left_tok in
    match val_ with
    | None ->
      let right = Token.pp ~inside:loc ~after:idx right_tok in
      flow (break 0) arr [ dot; left ^^ idx ^^ right ]
    | Some val_ ->
      let value = pp val_ in
      let right = Token.pp ~after:idx ~before:value right_tok in
      let larrow = Token.pp ~after:right ~before:value LESSMINUS in
      let access = flow (break 0) arr [ dot; left ^^ idx ^^ right ] in
      prefix ~indent:2 ~spaces:1 (group (access ^/^ larrow)) value

  and pp_access ~loc arr paren idx val_ =
    pp_gen_access ~loc paren arr (pp idx) val_

  and pp_dotop_access ~loc accessed path op paren indices val_ =
    let indices =
      match indices with
      | [] -> assert false (* I think *)
      | idx :: ids ->
        List_like.pp_fields ~formatting:Wrap
          (pp idx) (List.map pp ids)
    in
    let path = Option.map Longident.pp path in
    pp_gen_access ~loc ?path ~dot:(str op) paren accessed indices val_

  (* TODO: add formating options *)
  and pp_if_then =
    If_then_else.knr_if_then

  and pp_if_then_else =
    If_then_else.knr_if_then_else

  and pp_sequence ~ext_attrs:(extension, attrs) e1 e2 =
    let compact =
      match !Options.Sequences.compact with
      | Multi -> false
      | Compact -> true
      | Compact_under_app -> false (* FIXME: under_app ps *)
    in
    let e1 = pp e1 in
    let e2 = pp e2 in
    let semi =
      Keyword.decorate (Token.pp ~after:e1 ~before:e2 SEMI)
        ~extension attrs
    in
    let doc =
      if compact then
        e1 ^^ semi ^/^ e2
      else
        e1 ^^ semi ^//^ e2
    in
    doc

  and pp_while ~(loc:Location.t) ~ext_attrs:(extension, attrs) cond body =
    let cond = pp cond in
    let body = pp body in
    let do_ = Token.pp ~after:cond ~before:body DO in
    let while_ =
      let token = Token.pp ~inside:loc ~before:cond WHILE in
      Keyword.decorate token ~extension attrs
    in
    let done_ = Token.pp ~inside:loc ~after:body DONE in
    let doc =
      group
        (group (while_ ^^ nest 2 (break 1 ^^ cond) ^/^ do_) ^^
          nest 2 (break 1 ^^ body) ^/^ done_)
    in
    doc

  and pp_for
      ~(loc:Location.t) ~ext_attrs:(extension, attrs) it start stop dir body
  =
    let it = Pattern.pp it in
    let start = pp start in
    let equals = Token.pp ~after:it ~before:start EQUAL in
    let stop = pp stop in
    let dir =
      Token.pp ~after:start ~before:stop
        (match dir with
        | Upto -> TO
        | Downto -> DOWNTO)
    in
    let body = pp body in
    let do_ = Token.pp ~after:stop ~before:body DO in
    let for_ = 
      let token = Token.pp ~inside:loc ~before:it FOR in
      Keyword.decorate token ~extension attrs
    in
    let done_ = Token.pp ~inside:loc ~after:body DONE in
    let doc =
      group
        (group
          (for_ ^^
            nest 2
              (break 1 ^^ (group (it ^/^ equals ^/^ start)) ^/^ dir ^/^ stop) ^/^
              do_) ^^
          nest 2 (break 1 ^^ body) ^/^ done_)
    in
    doc

  and pp_constraint ~loc exp ct =
    let exp = pp exp in
    let ct = Core_type.pp ct in
    let colon = Token.pp ~after:exp ~before:ct COLON in
    group (parens ~loc (exp ^/^ colon ^/^ ct))

  and pp_coerce ~loc exp ct_start ct =
    let exp = pp exp in
    let ct = Core_type.pp ct in
    let ct_start =
      match ct_start with
      | None -> empty
      | Some ct ->
        let ct = Core_type.pp ct in
        let colon = Token.pp ~after:exp ~before:ct COLON in
        break 1 ^^ colon ^/^ ct
    in
    let coerce = Token.pp ~after:exp ~before:ct COLONGREATER in
    group (parens ~loc (group (exp ^^ ct_start) ^/^ coerce ^/^ ct))

  and pp_send exp met =
    let exp = pp exp in
    let met = str met in
    let sharp = Token.pp ~after:exp ~before:met HASH in
    let doc = flow (break 0) exp [ sharp; met ] in
    doc

  and pp_new ~loc ~ext_attrs:(extension, attrs) lid =
    let lid = Longident.pp lid in
    let new_ = Token.pp ~inside:loc ~before:lid NEW in
    let new_ = Keyword.decorate new_ ~extension attrs in
    (group (new_ ^/^ lid))

  and pp_setinstvar lbl exp =
    let lbl = str lbl in
    let exp = pp exp in
    let larrow = Token.pp ~after:lbl ~before:exp LESSMINUS in
    let doc = lbl ^/^ larrow ^/^ exp in
    doc

  and obj_field_override (lbl, exp) =
    let fld = str lbl in
    match exp.pexp_desc with
    | Pexp_ident Lident s when s.txt = lbl.txt -> fld
    | _ ->
      let exp = pp exp in
      let equals = Token.pp ~after:fld ~before:exp EQUAL in
      fld ^/^ equals ^/^ exp

  and pp_override ~loc fields =
    List_like.pp ~loc ~formatting:!Options.Record.expression ~left:LBRACELESS
      ~right:GREATERRBRACE (List.map obj_field_override fields)

  and pp_letmodule
      ~loc ~ext_attrs:(extension, attrs) name (params, typ, mexp) expr
  =
    let binding = Module_binding.pp_raw name params typ mexp [] in
    let bind =
      let keyword =
        let let_ = Token.pp ~inside:loc ~before:binding.name LET in
        let mod_ = Token.pp ~after:let_ ~before:binding.name MODULE in
        Keyword.decorate (let_ ^/^ mod_) ~extension attrs
      in
      Binding.Module.pp ~loc ~keyword ~context:Struct binding
    in
    let expr = pp expr in
    let in_ = Token.pp ~after:bind ~before:expr IN in
    let doc = bind ^/^ in_ ^/^ expr in
    doc

  and pp_letexception ~(loc:Location.t) ~ext_attrs:(extension, attrs) exn exp =
    let exn = Constructor_decl.pp_extension exn in
    let exp = pp exp in
    let keyword =
      let let_ = Token.pp ~inside:loc ~before:exn LET in
      let exc = Token.pp ~after:let_ ~before:exn EXCEPTION in
      Keyword.decorate (let_ ^/^ exc) ~extension attrs
    in
    let in_ = Token.pp ~after:exn ~before:exp IN in
    let doc =
      group (prefix ~indent:2 ~spaces:1 keyword (group (exn ^/^ in_))) ^/^ exp
    in
    doc

  and pp_assert ~(loc:Location.t) ~ext_attrs:(extension, attrs) exp =
    let exp = pp exp in
    let assert_ =
      let kw = Token.pp ~inside:loc ~before:exp ASSERT in
      Keyword.decorate kw ~extension attrs
    in
    let doc = prefix ~indent:2 ~spaces:1 assert_ exp in
    doc

  and pp_lazy ~(loc:Location.t) ~ext_attrs:(extension, attrs) exp =
    let exp = pp exp in
    let lazy_ =
      let kw = Token.pp ~inside:loc ~before:exp LAZY in
      Keyword.decorate kw ~extension attrs
    in
    let doc = prefix ~indent:2 ~spaces:1 lazy_ exp in
    doc

  and pp_object ~loc ~ext_attrs cs =
    Class_structure.pp ~loc ~ext_attrs cs

  and pp_pack ~loc ~ext_attrs:(extension, attrs) me pkg =
    let me = Module_expr.pp me in
    let with_constraint =
      match pkg with
      | None -> me
      | Some pkg ->
        let constr = Core_type.Package_type.pp pkg in
        let colon = Token.pp ~after:me ~before:constr COLON in
        me ^/^ colon ^/^ constr
    in
    let mod_ = Token.pp ~inside:loc ~before:with_constraint MODULE in
    let mod_ = Keyword.decorate mod_ ~extension attrs in
    parens ~loc (mod_ ^/^ with_constraint)

  and pp_open ~loc lid exp =
    let lid = Longident.pp lid in
    let exp =
      match exp.pexp_desc with
      | Pexp_parens { begin_end = false; exp } ->
        parens ~loc ((nest 2 @@ break 0 ^^ pp exp) ^^ break 0)
      | _ -> pp exp
    in
    let dot = Token.pp ~after:lid ~before:exp DOT in
    lid ^^ dot ^^ exp

  and pp_letopen ~(loc:Location.t) ~ext_attrs od exp =
    let od = Open_declaration.pp ~ext_attrs Attached_to_item od in
    let exp = pp exp in
    let in_ = Token.pp ~after:od ~before:exp IN in
    let let_ = Token.pp ~inside:loc ~before:od LET in
    let doc = group (let_ ^/^ od ^/^ in_) ^/^ exp in
    doc

  and pp_binding_op (bop : binding_op) =
    let binding = Value_binding.pp_bop Attached_to_item bop in
    let keyword = Longident.pp_ident bop.pbop_op in
    Binding.pp ~keyword binding

  and pp_letop { let_; ands; body } =
    let let_ = pp_binding_op let_ in
    let ands = List.map pp_binding_op ands in
    let bindings = separate hardline let_ ands in
    let body = pp body in
    let in_ = Token.pp ~after:bindings ~before:body IN in
    (group (bindings ^/^ in_) ^^ hardline ^^ body)

  let () =
    If_then_else.imported_pp_exp := pp;
    Attribute.Payload.pp_expression := pp
end


and Fun_param : sig
  val pp : fun_param -> document
end = struct
  (* See Parser.labeled_simple_pattern *)
  let labeled_simple_pattern ~loc:sloc lbl default pat_and_type parentheses =
    match lbl, parentheses, pat_and_type, default with
    | Optional { name; extra_info = `Previous_token loc }, true, (None, ct), _
      ->
      let question = string ~loc "?" in
      let name = str name in
      let lparen = Token.pp ~after:question ~before:name LPAREN in
      let label_let_pattern =
        match ct with
        | None -> name
        | Some ct ->
          let ct = Core_type.pp ct in
          let colon = Token.pp ~after:name ~before:ct COLON in
          group (name ^^ colon ^^ break 0 ^^ ct)
      in
      let with_default =
        match default with
        | None -> label_let_pattern
        | Some exp ->
          let exp = Expression.pp exp in
          let equal = Token.pp ~after:label_let_pattern ~before:exp EQUAL in
          group (label_let_pattern ^^ equal ^^ break 0 ^^ exp)
      in
      let rparen = Token.pp ~inside:sloc ~after:with_default RPAREN in
      question ^^ lparen ^^ with_default ^^ rparen
    | Optional { name; extra_info = `Previous_token loc },
      false, (None, None), None ->
      group (string ~loc "?" ^^ str name)
    | Optional { name; extra_info = `Single_token }, true,
      (Some pat, ct), default ->
      let lbl = str name in
      let pat = Pattern.pp pat in
      let lparen = Token.pp ~after:lbl ~before:pat LPAREN in
      let let_pattern =
        match ct with
        | None -> pat
        | Some ct ->
          let ct = Core_type.pp ct in
          let colon = Token.pp ~after:pat ~before:ct COLON in
          group (pat ^^ colon ^^ break 0 ^^ ct)
      in
      let with_default =
        match default with
        | None -> let_pattern
        | Some exp ->
          let exp = Expression.pp exp in
          let equal = Token.pp ~after:let_pattern ~before:exp EQUAL in
          group (let_pattern ^^ equal ^^ break 0 ^^ exp)
      in
      let rparen = Token.pp ~inside:sloc ~after:with_default RPAREN in
      lbl ^^ lparen ^^ with_default ^^ rparen
    | Optional { name; extra_info = `Single_token },
      false, (Some pat, None), None ->
      str name ^^ Pattern.pp pat
    | Labelled { name; extra_info = `Previous_token loc }, true,
      (None, ct), None ->
      let tilde = string ~loc "~" in
      let name = str name in
      let lparen = Token.pp ~after:tilde ~before:name LPAREN in
      let label_let_pattern =
        match ct with
        | None -> name
        | Some ct ->
          let ct = Core_type.pp ct in
          let colon = Token.pp ~after:name ~before:ct COLON in
          group (name ^^ colon ^^ break 0 ^^ ct)
      in
      let rparen = Token.pp ~inside:sloc ~after:label_let_pattern RPAREN in
      tilde ^^ lparen ^^ label_let_pattern ^^ rparen
    | Labelled { name; extra_info = `Previous_token loc },
      false, (None, None), None ->
      group (string ~loc "~" ^^ str name)
    | Labelled { name; extra_info = `Single_token }, false, (Some pat, None),
      None ->
      str name ^^ Pattern.pp pat
    | Nolabel, false, (Some pat, None), None ->
      Pattern.pp pat
    | _ -> assert false

  let term = labeled_simple_pattern

  let newtype ~loc typ =
    let typ = flow_map (break 1) str (List.hd typ) (List.tl typ) in
    let kw = Token.pp ~inside:loc ~before:typ TYPE in
    parens ~loc (kw ^/^ typ)

  let pp = function
    | Term { loc; lbl; default; pat_with_annot; parens } ->
      group (term ~loc lbl default pat_with_annot parens)
    | Type (loc, typ) -> group (newtype ~loc typ)
end


and Value_binding : sig
  val pp_bop : Attribute.kind -> binding_op -> Binding.t
  val pp : Attribute.kind -> value_binding -> Binding.t
end = struct
  let pp_rhs
      ~attr_kind ~attrs
      ({ pexp_desc; pexp_loc; pexp_ext_attributes; pexp_attributes; _ } as e)
  =
    match pexp_desc with
    | Pexp_function (c :: cs) ->
      let kw, cases =
        Expression.function_chunks ~compact:false ~ext_attrs:pexp_ext_attributes
          ~loc:pexp_loc c cs
      in
      let cases = Attribute.attach_to_item cases pexp_attributes in
      let cases = Attribute.attach attr_kind cases attrs in
      Binding.Rhs.Two_parts (kw, cases)
    | _ ->
      let rhs = Attribute.attach attr_kind (Expression.pp e) attrs in
      Binding.Rhs.Regular rhs

  let pp_raw attr_kind pvb_pat pvb_params pvb_type pvb_expr pvb_attributes =
    let pat = Pattern.pp pvb_pat in
    let params = List.map Fun_param.pp pvb_params in
    let constr, coerce = pvb_type in
    let constr = Option.map Core_type.pp constr in
    let coerce = Option.map Core_type.pp coerce in
    let rhs = pp_rhs ~attr_kind ~attrs:pvb_attributes pvb_expr in
    { Binding.lhs = pat; params; constr; coerce; rhs }

  let pp_bop attr_kind { pbop_pat; pbop_params; pbop_type; pbop_exp; _ } =
    pp_raw attr_kind pbop_pat pbop_params pbop_type pbop_exp []

  let pp
      attr_kind { pvb_pat; pvb_params; pvb_type; pvb_expr; pvb_attributes; _ }
  =
    pp_raw attr_kind pvb_pat pvb_params pvb_type pvb_expr pvb_attributes
end


and Functor_param : sig
  val pp : functor_parameter loc -> document
end = struct
  let pp { loc; txt } =
    match txt with
    | Unit -> string ~loc "()"
    | Named (name, mty) ->
      let mty = Module_type.pp mty in
      let pp name =
        let colon = Token.pp ~after:name ~before:mty COLON in
        parens ~loc (group (name ^/^ colon) ^/^ mty)
      in
      match name.txt with
      | None ->
        let name = Token.pp ~inside:loc ~before:mty UNDERSCORE in
        pp name
      | Some s ->
        let name = string ~loc:name.loc s in
        pp name
end


and Module_expr : sig
  val pp : module_expr -> document
end = struct
  let rec pp { pmod_desc; pmod_attributes; pmod_loc } =
    let doc = pp_desc ~loc:pmod_loc pmod_desc in
    Attribute.attach_to_item doc pmod_attributes

  and pp_desc ~loc = function
    | Pmod_ident lid -> Longident.pp lid
    | Pmod_parens me -> parens ~loc (pp me)
    | Pmod_structure (attrs, str) -> pp_structure ~loc ~attrs str
    | Pmod_functor (attrs, params, me) -> pp_functor ~loc ~attrs params me
    | Pmod_apply (me1, me2) -> pp_apply ~loc me1 me2
    | Pmod_gen_apply me -> pp_gen_apply ~loc me
    | Pmod_constraint (me, mty) -> pp_constraint me mty
    | Pmod_unpack (attrs, e, pkg1, pkg2) -> pp_unpack ~loc ~attrs e pkg1 pkg2
    | Pmod_extension ext -> Attribute.Extension.pp ~loc Item ext

  and pp_structure ~loc ~attrs = function
    | [] -> Empty_delimited.pp ~loc attrs STRUCT END
    | si :: st ->
      let str = Structure.pp_nonempty si st in
      let struct_ =
        let token = Token.pp ~inside:loc ~before:str STRUCT in
        Keyword.decorate token ~extension:None attrs
      in
      let end_ = Token.pp ~inside:loc ~after:str END in
      group ((prefix ~indent:2 ~spaces:1 struct_ str) ^/^ end_)

  and pp_functor ~(loc:Location.t) ~attrs params me =
    let params =
      separate_map (break 1) ~f:Functor_param.pp (List.hd params)
        (List.tl params)
    in
    let me = pp me in
    let functor_ =
      let token = Token.pp ~inside:loc ~before:params FUNCTOR in
      Keyword.decorate token ~extension:None attrs
    in
    let arrow = Token.pp ~after:params ~before:me MINUSGREATER in
    functor_ ^/^ params ^/^ arrow ^/^ me

  and pp_apply ~loc me1 me2 =
    let me1 = pp me1 in
    let me2 = pp me2 in
    let rparen = Token.pp ~inside:loc ~after:me2 RPAREN in
    let lparen = Token.pp ~after:me1 ~before:me2 LPAREN in
    me1 ^^ break 0 ^^ (lparen ^^ me2 ^^ rparen)

  and pp_gen_apply ~loc me =
    let me = pp me in
    let rparen = Token.pp ~inside:loc ~after:me RPAREN in
    let lparen = Token.pp ~after:me ~before:rparen LPAREN in
    me ^^ break 0 ^^ (lparen ^^ rparen)

  and pp_constraint me mty =
    let me = pp me in
    let mty = Module_type.pp mty in
    Two_separated_parts.sep_with_first me ~sep:COLON mty

  and pp_unpack ~(loc:Location.t) ~attrs exp pkg1 pkg2 =
    let exp = Expression.pp exp in
    let val_exp =
      let tok = Token.pp ~inside:loc ~before:exp VAL in
      prefix ~spaces:1 ~indent:2
        (Keyword.decorate tok ~extension:None attrs) exp
    in
    let with_annot =
      match pkg1 with
      | None -> val_exp
      | Some pkg ->
        let pkg = Core_type.Package_type.pp pkg in
        let colon = Token.pp ~after:val_exp ~before:pkg COLON in
        prefix ~spaces:1 ~indent:2 val_exp (group (colon ^/^ pkg))
    in
    match pkg2 with
    | None -> with_annot
    | Some pkg ->
      let pkg = Core_type.Package_type.pp pkg in
      let coerce = Token.pp ~after:val_exp ~before:pkg COLONGREATER in
      prefix ~spaces:1 ~indent:2 with_annot (group (coerce ^/^ pkg))
end


and Module_type : sig
  val pp : module_type -> document
end = struct
  let rec pp { pmty_desc; pmty_attributes; pmty_loc; _ } =
    let doc = group (pp_desc ~loc:pmty_loc pmty_desc) in
    Attribute.attach_to_item doc pmty_attributes

  and pp_desc ~loc = function
    | Pmty_alias lid (* [module type _ = A] *)
    | Pmty_ident lid (* [module _ : A] *)
      ->
      Longident.pp lid
    | Pmty_signature sg -> pp_signature ~loc sg
    | Pmty_functor (attrs, params, mty) -> pp_functor ~loc ~attrs params mty
    | Pmty_with (mty, cstrs) -> pp_with mty cstrs
    | Pmty_typeof (attrs, me) -> pp_typeof ~loc ~attrs me
    | Pmty_extension ext -> Attribute.Extension.pp ~loc Item ext
    | Pmty_parens mty -> parens ~loc (pp mty)

  and pp_signature ~loc = function
    | [] -> Empty_delimited.pp ~loc [] SIG END
    | si :: sg ->
      let sg = Signature.pp_nonempty si sg in
      let sig_ = Token.pp ~inside:loc ~before:sg SIG in
      let end_ = Token.pp ~inside:loc ~after:sg END in
      (prefix ~indent:2 ~spaces:1 sig_ sg) ^/^ end_

  and pp_short_functor param_mty res_mty =
    let param_mty = Module_type.pp param_mty in
    let res_mty = Module_type.pp res_mty in
    let arrow = Token.pp ~after:param_mty ~before:res_mty MINUSGREATER in
    param_mty ^/^ arrow ^/^ res_mty

  and pp_regular_functor ~(loc:Location.t) ~attrs params mty =
    let params =
      separate_map (break 1) ~f:Functor_param.pp (List.hd params)
        (List.tl params)
    in
    let mty = pp mty in
    let functor_ =
      let tok = Token.pp ~inside:loc ~before:params FUNCTOR in
      Keyword.decorate tok ~extension:None attrs
    in
    let arrow = Token.pp ~after:params ~before:mty MINUSGREATER in
    functor_ ^/^ params ^/^ arrow ^/^ mty

  and pp_functor ~loc ~attrs params mty =
    match attrs, params with
    | [], [ { txt = Named ({ txt = None; _ }, param_mty); _ } ] ->
      pp_short_functor param_mty mty
    | _ -> pp_regular_functor ~loc ~attrs params mty

  and attach_constraint mty is_first_cstr (kw, cstr) =
    let cstr =
      match cstr with
      | Pwith_type (lid, td) ->
        let lid = Longident.pp lid in
        let keyword =
          Token.pp ~after:mty ~before:lid
            (match kw with
            | With -> WITH
            | And -> AND)
        in
        Type_declaration.pp_with_constraint ~override_name:lid
          ~keyword:(With_type keyword) td
      | Pwith_typesubst (lid, td) ->
        let lid = Longident.pp lid in
        let keyword =
          Token.pp ~after:mty ~before:lid
            (match kw with
            | With -> WITH
            | And -> AND)
        in
        Type_declaration.pp_with_constraint ~binder:COLONEQUAL
          ~override_name:lid ~keyword:(With_type keyword) td
      | Pwith_module (lid1, lid2) ->
        let d1 = Longident.pp lid1 in
        let d2 = Longident.pp lid2 in
        let keyword =
          Token.pp ~after:mty ~before:d1
            (match kw with
            | With -> WITH
            | And -> AND)
        in
        let module_ = Token.pp ~after:keyword ~before:d1 MODULE in
        let keyword = keyword ^/^ module_ in
        Binding.pp_simple ~keyword d1 d2
      | Pwith_modsubst (lid1, lid2) ->
        let d1 = Longident.pp lid1 in
        let d2 = Longident.pp lid2 in
        let keyword =
          Token.pp ~after:mty ~before:d1
            (match kw with
            | With -> WITH
            | And -> AND)
        in
        let module_ = Token.pp ~after:keyword ~before:d1 MODULE in
        let keyword = keyword ^/^ module_ in
        Binding.pp_simple ~binder:COLONEQUAL ~keyword d1 d2
      | Pwith_modtype (lid, mty') ->
        let d1 = Longident.pp lid in
        let d2 = Module_type.pp mty' in
        let keyword =
          Token.pp ~after:mty ~before:d1
            (match kw with
            | With -> WITH
            | And -> AND)
        in
        let module_ = Token.pp ~after:keyword ~before:d1 MODULE in
        let type_ = Token.pp ~after:module_ ~before:d1 TYPE in
        let keyword = keyword ^/^ module_ ^/^ type_ in
        Binding.pp_simple ~keyword d1 d2
      | Pwith_modtypesubst (lid, mty') ->
        let d1 = Longident.pp lid in
        let d2 = Module_type.pp mty' in
        let keyword =
          Token.pp ~after:mty ~before:d1
            (match kw with
            | With -> WITH
            | And -> AND)
        in
        let module_ = Token.pp ~after:keyword ~before:d1 MODULE in
        let type_ = Token.pp ~after:module_ ~before:d1 TYPE in
        let keyword = keyword ^/^ module_ ^/^ type_ in
        Binding.pp_simple ~binder:COLONEQUAL ~keyword d1 d2
    in
    if is_first_cstr then
      prefix ~spaces:1 ~indent:2 mty cstr
    else
      let indent =
        match kw with
        | With -> 2
        | And -> 3
      in
      mty ^^ nest indent (break 1 ^^ cstr)

  and pp_with mty cstrs =
    let mty = pp mty in
    let with_constraints, _ =
      List.fold_left (fun (mty, is_first) cstr ->
        let mty = attach_constraint mty is_first cstr in
        mty, false
      ) (mty, true) cstrs
    in
    with_constraints

  and pp_typeof ~loc ~attrs exp =
    let me = Module_expr.pp exp in
    let module_ = Token.pp ~inside:loc ~before:me MODULE in
    let type_ = Token.pp ~after:module_ ~before:me TYPE in
    let of_ =
      let tok = Token.pp ~after:type_ ~before:me OF in
      Keyword.decorate tok ~extension:None attrs
    in
    flow (break 1) module_ [ type_; of_; me ]
end


and Module_binding : sig
  val pp_raw
    :  string option loc
    -> functor_parameter loc list
    -> module_type option
    -> module_expr
    -> attributes
    -> Binding.Module.t

  val pp : module_binding -> Binding.Module.t
  val param : functor_parameter loc -> t
end = struct
  let param { loc; txt } =
    match txt with
    | Unit -> string ~loc "()"
    | Named (name, mty) ->
      let name = module_name name in
      let mty = Module_type.pp mty in
      let colon = Token.pp ~after:name ~before:mty COLON in
      group (
        parens ~loc (prefix ~indent:2 ~spaces:1 (group (name ^/^ colon)) mty)
      )

  let pp_mty = function
    | None -> Binding.Module.None
    | Some ({ pmty_desc; pmty_attributes; _ } as mty) ->
      match pmty_desc, pmty_attributes with
      | Pmty_signature (si :: sg), [] ->
        Binding.Module.Sig (Signature.pp_nonempty si sg)
      | _ -> Binding.Module.Mty (Module_type.pp mty)

  let pp_me ({ pmod_desc; pmod_attributes; _ } as me) =
    match pmod_desc, pmod_attributes with
    | Pmod_structure (_, si :: st), [] ->
      (* FIXME: attrs ^ ? *)
      Binding.Module.Items (Structure.pp_nonempty si st)
    | _ -> Binding.Module.Generic (Module_expr.pp me)

  let pp_raw name params mty me attrs =
    let name = module_name name in
    let params = List.map param params in
    let constr = pp_mty mty in
    let body = pp_me me in
    let attributes =
      match attrs with
      | [] -> empty
      | attr :: attrs ->
        separate_map (break 0) ~f:(Attribute.pp Attached_to_structure_item) attr
          attrs
    in
    { Binding.Module.name; params; constr; body; attributes }

  let pp { pmb_name; pmb_params; pmb_type; pmb_expr; pmb_attributes; _ } =
    let binding = pp_raw pmb_name pmb_params pmb_type pmb_expr pmb_attributes in
    let body = binding.body in
    { binding with  body }
end


and Module_declaration : sig
  val pp : module_declaration -> document

  val pp_raw : module_declaration -> Binding.Module.t
  val decide_context : module_type -> Binding.Module.context
end = struct
  let pp_mty ({ pmty_desc; pmty_attributes; _ } as mty) =
    match pmty_desc, pmty_attributes with
    | Pmty_signature (si :: sg), [] ->
      Binding.Module.Items (Signature.pp_nonempty si sg)
    | _ -> Binding.Module.Generic (Module_type.pp mty)

  let pp_raw { pmd_name; pmd_params; pmd_type; pmd_attributes; _ } =
    let name = module_name pmd_name in
    let params = List.map Module_binding.param pmd_params in
    let body = pp_mty pmd_type in
    let attributes =
      match pmd_attributes with
      | [] -> empty
      | attr :: attrs ->
        separate_map (break 0) ~f:(Attribute.pp Attached_to_structure_item) attr
          attrs
    in
    { Binding.Module.name; params; constr = None; body; attributes }

  let decide_context pmd_type : Binding.Module.context =
    (* This is a hack.
       The context is used to decide whether to print "=" or ":", but that
       doesn't quite work: module aliases declarations use "=". *)
    match pmd_type.pmty_desc with
    | Pmty_alias _ -> Struct
    | _ -> Sig

  let pp pmd =
    let loc = pmd.pmd_loc in
    let kw =
      let (extension, attrs) = pmd.pmd_ext_attributes in
      Keyword.decorate (Token.pp ~inside:loc MODULE) ~extension attrs
    in
    let text, pmd =
      let text, attrs =
        Attribute.extract_text ~item_start_pos:loc.loc_start
          pmd.pmd_attributes
      in
      text, { pmd with  pmd_attributes = attrs }
    in
    let binding = pp_raw pmd in
    let context = decide_context pmd.pmd_type in
    let binding = Binding.Module.pp ~loc ~keyword:kw ~context binding in
    let docs = Attribute.prepend_text text binding in
    separate (twice hardline) (List.hd docs) (List.tl docs)
end


and Module_substitution : sig
  val pp
    :  ext_attrs:string loc option * attributes
    -> module_substitution
    -> document
end = struct
  let pp
      ~ext_attrs:(extension, attrs)
      { pms_name; pms_manifest; pms_attributes; pms_loc }
  =
    let kw =
      Keyword.decorate (Token.pp ~inside:pms_loc MODULE) ~extension attrs
    in
    let name = str pms_name in
    let man = Longident.pp pms_manifest in
    let doc = Binding.pp_simple ~keyword:kw name ~binder:COLONEQUAL man in
    Attribute.attach_to_top_item doc pms_attributes
end


and Module_type_declaration : sig
  val pp
    :  ext_attrs:(string loc option * attributes)
    -> ?subst:bool
    -> module_type_declaration
    -> document
end = struct
  let pp
      ~ext_attrs:(extension, attrs) ?(subst=false)
      { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
  =
    let text, pmtd_attributes =
      Attribute.extract_text pmtd_attributes ~item_start_pos:pmtd_loc.loc_start
    in
    let name = str pmtd_name in
    let kw =
      let module_ = Token.pp ~inside:pmtd_loc ~before:name MODULE in
      let type_ = Token.pp ~after:module_ ~before:name TYPE in
      group (module_ ^/^ (Keyword.decorate type_ ~extension attrs))
    in
    let doc =
      match pmtd_type with
      | None -> group (kw ^/^ name)
      | Some mty ->
        let typ = Module_type.pp mty in
        Binding.pp_simple ~keyword:kw name typ
          ~binder:(if subst then COLONEQUAL else EQUAL)
    in
    let decl =
      Attribute.attach_to_top_item doc pmtd_attributes |>
        Attribute.prepend_text text
    in
    separate (twice hardline) (List.hd decl) (List.tl decl)
end


and Structure : sig
  val pp_nonempty : structure_item -> structure -> document
  val ends_in_obj : structure -> bool
end = struct
  let ends_in_obj lst =
    match list_last lst with
    | None -> false
    | Some { pstr_desc; _ } ->
      match pstr_desc with
      | Pstr_type (_, decls) -> Type_declaration.ends_in_obj decls
      | Pstr_typext te -> Type_extension.ends_in_obj te
      | Pstr_exception exn -> Type_exception.ends_in_obj exn
      | Pstr_eval _
      | Pstr_value _
      | Pstr_primitive _
      | Pstr_module _
      | Pstr_recmodule _
      | Pstr_modtype _
      | Pstr_open _
      | Pstr_include _
      | Pstr_attribute _
      | Pstr_extension _
      | Pstr_class _
      | Pstr_class_type _
        ->
        false

  let pp_eval ~first exp attrs =
    let exp = Expression.pp exp in
    let doc = Attribute.attach_to_top_item exp attrs in
    (* FIXME: the ";;" must exist in the source, right? *)
    if first then doc else PPrint.string ";; " ++ doc

  let pp_value ~loc ~ext_attrs:(extension, attrs1) rf vbs =
    assert (attrs1 = []);
    let vbs =
      let previous_vb = ref None in
      List.concat_map (fun vb ->
        let text, vb =
          let text, attrs =
            Attribute.extract_text ~item_start_pos:vb.pvb_loc.loc_start
              vb.pvb_attributes
          in
          text, { vb with  pvb_attributes = attrs }
        in
        let binding = Value_binding.pp Attached_to_structure_item vb in
        let keyword =
          let lhs = binding.lhs in
          let attrs =
            match vb.pvb_ext_attributes with
            | Some _, _ -> assert false
            | None, attrs -> attrs
          in
          let token, modifier =
            match !previous_vb with
            | None ->
              Keyword.decorate (Token.pp ~inside:loc ~before:lhs LET) ~extension
                [],
              rec_token ~recursive_by_default:false rf
            | Some prev_vb -> Token.pp ~after:prev_vb ~before:lhs AND, None
          in
          let kw = Keyword.decorate token ~extension:None attrs in
          match modifier with
          | None -> kw
          | Some tok ->
            let modif = Token.pp ~after:kw ~before:lhs tok in
            kw ^/^ modif
        in
        let binding = Binding.pp ~keyword binding in
        previous_vb := Some binding;
        Attribute.prepend_text text binding
      ) vbs
    in
    separate (twice hardline) (List.hd vbs) (List.tl vbs)

  let pp_modules ~loc rf mbs =
    let mbs =
      let previous_mb = ref None in
      List.concat_map (fun mb ->
        let text, mb =
          let text, attrs =
            Attribute.extract_text mb.pmb_attributes
              ~item_start_pos:mb.pmb_loc.loc_start
          in
          text, { mb with  pmb_attributes = attrs }
        in
        let binding = Module_binding.pp mb in
        let keyword =
          let lhs = binding.name in
          let kw, modifier =
            match !previous_mb with
            | None ->
              Token.pp ~inside:loc ~before:lhs MODULE,
              rec_token ~recursive_by_default:false rf
            | Some prev_mb -> Token.pp ~after:prev_mb ~before:lhs AND, None
          in
          let extension, attrs = mb.pmb_ext_attributes in
          let kw = Keyword.decorate kw ~extension attrs in
          match modifier with
          | None -> kw
          | Some tok ->
            let modif = Token.pp ~after:kw ~before:lhs tok in
            kw ^/^ modif
        in
        let binding =
          Binding.Module.pp ~loc:mb.pmb_loc ~context:Struct ~keyword binding
        in
        previous_mb := Some binding;
        Attribute.prepend_text text binding
      ) mbs
    in
    separate (twice hardline) (List.hd mbs) (List.tl mbs)

  let pp_module ~loc mb = pp_modules ~loc Nonrecursive [ mb ]

  let pp_recmodule ~loc mbs = pp_modules ~loc Recursive mbs

  let pp_include
      ~ext_attrs:(extension, attrs) { pincl_mod; pincl_attributes; pincl_loc }
  =
    let incl = Module_expr.pp pincl_mod in
    let kw =
      Keyword.decorate (Token.pp ~inside:pincl_loc ~before:incl INCLUDE)
        ~extension attrs
    in
    Attribute.attach_to_top_item (prefix ~indent:2 ~spaces:1 kw incl)
      pincl_attributes

  let pp_extension ~loc ext attrs =
    let ext = Attribute.Extension.pp ~loc Structure_item ext in
    Attribute.attach_to_top_item ext attrs

  let pp_item
      ?(first=false)
      { pstr_desc; pstr_loc = loc; pstr_ext_attributes = ext_attrs }
  =
    match pstr_desc with
    | Pstr_eval (e, attrs) -> pp_eval ~first e attrs
    | Pstr_value (rf, vbs) -> pp_value ~loc ~ext_attrs rf vbs
    | Pstr_primitive vd -> Value_description.pp ~ext_attrs vd
    | Pstr_type (rf, tds) -> Type_declaration.pp_decl ~ext_attrs rf tds
    | Pstr_typext te -> Type_extension.pp ~ext_attrs te
    | Pstr_exception exn -> Type_exception.pp ~ext_attrs exn
    | Pstr_module mb -> pp_module ~loc mb
    | Pstr_recmodule mbs -> pp_recmodule ~loc mbs
    | Pstr_modtype mtd -> Module_type_declaration.pp ~ext_attrs mtd
    | Pstr_open od ->
      Open_declaration.pp ~ext_attrs Attached_to_structure_item od
    | Pstr_class cds -> Class_declaration.pp ~ext_attrs cds
    | Pstr_class_type ctds -> Class_type_declaration.pp ~ext_attrs ctds
    | Pstr_include incl -> pp_include ~ext_attrs incl
    | Pstr_attribute attr -> Attribute.pp Free_floating attr
    | Pstr_extension (ext, attrs) -> pp_extension ~loc ext attrs

  let rec group_by_desc acc = function
    | [] -> [ List.rev acc ]
    | i :: is ->
      if same_group i (List.hd acc) then
        group_by_desc (i :: acc) is
      else
        List.rev acc :: group_by_desc [ i ] is

  and same_group d1 d2 =
    match d1.pstr_desc, d2.pstr_desc with
    | Pstr_value _, Pstr_value _
    | Pstr_primitive _, Pstr_primitive _
    | Pstr_type _, Pstr_type _
    | Pstr_typext _, Pstr_typext _
    | Pstr_exception _, Pstr_exception _
    | Pstr_module _, Pstr_module _
    | Pstr_recmodule _, Pstr_recmodule _
    | Pstr_modtype _, Pstr_modtype _
    | Pstr_open _, Pstr_open _
    | Pstr_class _, Pstr_class _
    | Pstr_class_type _, Pstr_class_type _
    | Pstr_include _, Pstr_include _
    | Pstr_extension _, Pstr_extension _
      ->
      true
    | Pstr_attribute a1, Pstr_attribute a2 ->
      (Attribute.is_non_doc a1 && Attribute.is_non_doc a2) ||
        (not (Attribute.is_non_doc a1) && not (Attribute.is_non_doc a2))
    | _ -> false

  let pp_group ?(first=false) = function
    | [] -> assert false
    | x :: xs -> pp_item ~first x :: List.map pp_item xs

  let pp_groups = function
    | [] -> assert false
    | g :: gs -> pp_group ~first:true g :: List.map pp_group gs

  let pp_nonempty i is =
    match
      group_by_desc [ i ] is |> pp_groups |> List.map collate_toplevel_items
    with
    | [] -> assert false
    | [ doc ] -> doc
    | doc :: docs -> separate (twice hardline) doc docs

  let () =
    Attribute.Payload.(
      pp_struct := pp_nonempty;
      struct_ends_in_obj := ends_in_obj
    )
end


and Signature : sig
  val pp_nonempty : signature_item -> signature -> document
  val ends_in_obj : signature -> bool
end = struct
  let ends_in_obj lst =
    match list_last lst with
    | None -> false
    | Some { psig_desc; _ } ->
      match psig_desc with
      | Psig_value vd -> Core_type.ends_in_obj vd.pval_type
      | Psig_type (_, decls) | Psig_typesubst decls ->
        Type_declaration.ends_in_obj decls
      | Psig_typext te -> Type_extension.ends_in_obj te
      | Psig_exception exn -> Type_exception.ends_in_obj exn
      (* FIXME: any of these could have a with constraint. *)
      | Psig_module _
      | Psig_recmodule _
      | Psig_modsubst _
      | Psig_modtype _
      | Psig_modtypesubst _
      | Psig_open _
      | Psig_include _
      | Psig_attribute _
      | Psig_extension _
      | Psig_class _
      | Psig_class_type _
        ->
        false

  let pp_extension ~loc ext attrs =
    let ext = Attribute.Extension.pp ~loc Structure_item ext in
    Attribute.attach_to_top_item ext attrs

  let pp_include
      ~ext_attrs:(extension, attrs) { pincl_mod; pincl_attributes; pincl_loc }
  =
    let incl = Module_type.pp pincl_mod in
    let kw =
      Keyword.decorate (Token.pp ~inside:pincl_loc ~before:incl INCLUDE)
        ~extension attrs
    in
    Attribute.attach_to_top_item (group (kw ^/^ incl)) pincl_attributes

  let pp_recmodules mds =
    let mds =
      let i = ref 0 in
      List.concat_map (fun md ->
        let text, md =
          let text, attrs =
            Attribute.extract_text md.pmd_attributes
              ~item_start_pos:md.pmd_loc.loc_start
          in
          text, { md with  pmd_attributes = attrs }
        in
        let keyword =
          let fake_name = string "_" ~loc:md.pmd_name.loc in
          let decorate tok =
            let extension, attrs = md.pmd_ext_attributes in
            Keyword.decorate
              (Token.pp ~inside:md.pmd_loc ~before:fake_name tok) ~extension
              attrs
          in
          if !i = 0 then
            let module_ = decorate MODULE in
            group (module_ ^/^ Token.pp ~after:module_ ~before:fake_name REC)
          else
            decorate AND
        in
        incr i;
        let binding =
          Binding.Module.pp ~loc:md.pmd_loc (Module_declaration.pp_raw md)
            ~context:(Module_declaration.decide_context md.pmd_type) ~keyword
        in
        Attribute.prepend_text text binding
      ) mds
    in
    separate (twice hardline) (List.hd mds) (List.tl mds)

  let pp_item ({ psig_desc; psig_ext_attributes = ext_attrs; psig_loc = loc } as _item) =
    match psig_desc with
    | Psig_value vd -> Value_description.pp ~ext_attrs vd
    | Psig_type (rf, decls) -> Type_declaration.pp_decl ~ext_attrs rf decls
    | Psig_typesubst decls -> Type_declaration.pp_subst ~ext_attrs decls
    | Psig_typext te -> Type_extension.pp ~ext_attrs te
    | Psig_exception exn -> Type_exception.pp ~ext_attrs exn
    | Psig_module md -> Module_declaration.pp md
    | Psig_recmodule pmds -> pp_recmodules pmds
    | Psig_modsubst ms -> Module_substitution.pp ~ext_attrs ms
    | Psig_modtype mtd -> Module_type_declaration.pp ~ext_attrs mtd
    | Psig_modtypesubst mts ->
      Module_type_declaration.pp ~ext_attrs ~subst:true mts
    | Psig_open od -> Open_description.pp ~ext_attrs od
    | Psig_include incl -> pp_include ~ext_attrs incl
    | Psig_attribute attr -> Attribute.pp Free_floating attr
    | Psig_extension (ext, attrs) -> pp_extension ~loc ext attrs
    | Psig_class cds -> Class_description.pp ~ext_attrs cds
    | Psig_class_type ctds -> Class_type_declaration.pp ~ext_attrs ctds

  let rec group_by_desc acc = function
    | [] -> [ List.rev acc ]
    | i :: is ->
      if same_group i (List.hd acc) then
        group_by_desc (i :: acc) is
      else
        List.rev acc :: group_by_desc [ i ] is

  and same_group d1 d2 =
    match d1.psig_desc, d2.psig_desc with
    | Psig_value _, Psig_value _
    | Psig_type _, Psig_type _
    | Psig_typesubst _, Psig_typesubst _
    | Psig_typext _, Psig_typext _
    | Psig_exception _, Psig_exception _
    | Psig_module _, Psig_module _
    | Psig_recmodule _, Psig_recmodule _
    | Psig_modsubst _, Psig_modsubst _
    | Psig_modtype _, Psig_modtype _
    | Psig_open _, Psig_open _
    | Psig_include _, Psig_include _
    | Psig_attribute _, Psig_attribute _
    | Psig_extension _, Psig_extension _
    | Psig_class _, Psig_class _
    | Psig_class_type _, Psig_class_type _
      ->
      true
    | _ -> false

  let pp_nonempty i is =
    match
      group_by_desc [ i ] is |> List.map (List.map pp_item) |>
        List.map collate_toplevel_items
    with
    | [] -> assert false
    | [ doc ] -> doc
    | doc :: docs -> separate (twice hardline) doc docs

  let () =
    Attribute.Payload.(
      pp_sig := pp_nonempty;
      sig_ends_in_obj := ends_in_obj
    )
end


and Value_description : sig
  val pp
    : ext_attrs:string loc option * attributes -> value_description -> document
end = struct
  let pp ~ext_attrs:(extension, attrs) vd =
    let name = Longident.pp_ident vd.pval_name in
    let ctyp = Core_type.pp vd.pval_type in
    let tok, with_prim =
      match vd.pval_prim with
      | [] -> Parser.VAL, ctyp
      | p :: ps ->
        let prims =
          separate_map (break 1) ~f:(fun { loc; txt } ->
            Constant.pp_string_lit ~loc txt
          ) p ps
        in
        let equals = Token.pp ~after:ctyp ~before:prims EQUAL in
        Parser.EXTERNAL, ctyp ^/^ group (equals ^/^ prims)
    in
    let kw =
      Keyword.decorate (Token.pp ~inside:vd.pval_loc ~before:name tok)
        ~extension attrs
    in
    let colon = Token.pp ~after:name ~before:with_prim COLON in
    let doc =
      prefix ~indent:2 ~spaces:1 (prefix ~indent:2 ~spaces:1 kw name)
        (group (colon ^^ ifflat space (twice space) ^^ with_prim))
    in
    Attribute.attach_to_top_item doc vd.pval_attributes
end


and Type_extension : sig
  val pp
    : ext_attrs:string loc option * attributes -> type_extension -> document
  val ends_in_obj : type_extension -> bool
end = struct
  let constructors ~loc = function
    | [] -> assert false
    | c :: cs ->
      case_list ~loc (Constructor_decl.pp_extension c)
        (List.map Constructor_decl.pp_extension cs)

  let pp
      ~ext_attrs:(extension, attrs)
      { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
        ptyext_attributes;
        ptyext_loc }
  =
    let path = Longident.pp ptyext_path in
    let lhs =
      Type_declaration.with_params ~decl_loc:ptyext_loc ptyext_params path
    in
    let constructors = constructors ~loc:ptyext_loc ptyext_constructors in
    let rhs =
      match ptyext_private with
      | None -> constructors
      | Some loc -> group (string ~loc "private" ^/^ constructors)
    in
    let rhs = Attribute.attach_to_top_item rhs ptyext_attributes in
    let keyword =
      Keyword.decorate (Token.pp ~inside:ptyext_loc ~before:lhs TYPE) ~extension
        attrs
    in
    Binding.pp_simple ~keyword ~binder:PLUSEQ lhs rhs

  let ends_in_obj = function
    | { ptyext_attributes = []; ptyext_constructors; _ } ->
      begin match list_last ptyext_constructors with
      | Some { pext_attributes = []; pext_kind = Pext_decl (_, args, cto); _ }
        ->
        begin match cto with
        | Some ct -> Core_type.ends_in_obj ct
        | None ->
          match args with
          | Pcstr_tuple (_ :: _ as lst) ->
            let ct = Option.get (list_last lst) in
            Core_type.ends_in_obj ct
          | _ -> false
        end
      | _ -> false
      end
    | _ -> false
end


and Type_exception : sig
  val pp
    : ext_attrs:string loc option * attributes -> type_exception -> document
  val ends_in_obj : type_exception -> bool
end = struct
  let pp
      ~ext_attrs:(extension, attrs)
      { ptyexn_constructor; ptyexn_attributes; ptyexn_loc }
  =
    let cstr = Constructor_decl.pp_extension ptyexn_constructor in
    let kw =
      Keyword.decorate (Token.pp ~inside:ptyexn_loc ~before:cstr EXCEPTION)
        ~extension attrs
    in
    let doc = group (prefix ~spaces:1 ~indent:2 kw cstr) in
    Attribute.attach_to_top_item doc ptyexn_attributes

  let ends_in_obj = function
    | { ptyexn_attributes = []; ptyexn_constructor; _ } ->
      begin match ptyexn_constructor with
      | { pext_attributes = []; pext_kind = Pext_decl (_, args, cto); _ } ->
        begin match cto with
        | Some ct -> Core_type.ends_in_obj ct
        | None ->
          match args with
          | Pcstr_tuple (_ :: _ as lst) ->
            let ct = Option.get (list_last lst) in
            Core_type.ends_in_obj ct
          | _ -> false
        end
      | _ -> false
      end
    | _ -> false
end


and Type_declaration : sig
(*   val pp : type_declaration -> document * document *)
  val with_params
    :  decl_loc:Location.t
    -> ?always_enclosed:bool
    -> ?enclosing:(loc:Location.t -> document -> document)
    -> (core_type * variance_and_inj) list
    -> document
    -> document

  type keyword =
    | With_type of document
    | Type of rec_flag
    | And

  val pp_decl
    :  ext_attrs:string loc option * attributes
    -> rec_flag
    -> type_declaration list
    -> document

  val pp_subst
    :  ext_attrs:string loc option * attributes
    -> type_declaration list
    -> document

  val pp_with_constraint
    :  ?override_name:document
    -> ?binder:Source_parsing.Parser.token
    -> keyword:keyword
    -> type_declaration
    -> document

  val ends_in_obj : type_declaration list -> bool
end = struct
  let ends_in_obj lst =
    match list_last lst with
    | Some { ptype_attributes = []; ptype_cstrs = (_ :: _ as cstrs); _ } ->
      let _, ct, _ = Option.get @@ list_last cstrs in
      Core_type.ends_in_obj ct
    | Some
        { ptype_attributes = []; ptype_manifest = Some ct;
          ptype_kind = Ptype_abstract;
          _ }
      ->
      Core_type.ends_in_obj ct
    | Some { ptype_attributes = []; ptype_kind = Ptype_variant { txt; _ }; _ }
      ->
      begin match list_last txt with
      | Some { pcd_attributes = []; pcd_res = Some ct; _ } ->
        Core_type.ends_in_obj ct
      | Some { pcd_attributes = []; pcd_args = Pcstr_tuple cts; _ } ->
        Core_type.ends_in_obj (Option.get @@ list_last cts)
      | _ -> false
      end
    | _ -> false

  let pp_param (ct, variance_and_inj) =
    let ct = Core_type.pp ct in
    List.fold_right (fun vi acc -> str vi ^^ acc) variance_and_inj ct

  let with_params ~decl_loc ?(always_enclosed=false) ?(enclosing=parens) lst name =
    match lst with
    | [] -> name
    | [ x ] when always_enclosed ->
      group (enclosing ~loc:decl_loc (pp_param x) ^/^ name)
    | [ x ] -> group (pp_param x ^/^ name)
    | x :: xs ->
      let params = tuple_fields pp_param x xs in
      group (enclosing ~loc:decl_loc params ^/^ name)

  let label_declaration { pld_name; pld_mutable; pld_type; pld_attributes; _ } =
    let name = str pld_name in
    let typ = Core_type.pp pld_type in
    let colon = Token.pp ~after:name ~before:typ COLON in
    let lhs = group (name ^/^ colon) in
    let with_mutable_ =
      match pld_mutable with
      | Mutable loc -> group (string ~loc "mutable" ^/^ lhs)
      | Immutable -> lhs
    in
    let decl = group (nest 2 (with_mutable_ ^/^ typ)) in
    decl, List.map (Attribute.pp Attached_to_item) pld_attributes

  let record ~loc lbl_decls =
    (* FIXME: loc won't be use since the list is nonempty *)
    let fields = List.map label_declaration lbl_decls in
    Record_like.pp ~loc ~formatting:Fit_or_vertical (* never wrap decls *)
      ~left:LBRACE ~right:RBRACE fields

  let () =
    Constructor_decl.pp_record := record

  let variant { Location.loc; txt = cstrs } =
    match cstrs with
    | [] -> string ~loc "|"
    | cstr :: cstrs ->
      case_list ~loc
        (Constructor_decl.pp_decl cstr)
        (List.map (fun c -> nest 2 @@ Constructor_decl.pp_decl c) cstrs)

  let non_abstract_kind = function
    | Ptype_abstract -> assert false
    | Ptype_open loc -> string ~loc ".."
    | Ptype_record (loc, lbl_decls) -> record ~loc lbl_decls
    | Ptype_variant cstrs -> variant cstrs

  let pp_constraint (ct1, ct2, _) =
    let ct1 = Core_type.pp ct1 in
    let ct2 = Core_type.pp ct2 in
    let equals = Token.pp ~after:ct1 ~before:ct2 EQUAL in
    ct1 ^/^ equals ^/^ ct2

  let add_constraints decl = function
    | [] -> decl
    | cstr :: cstrs ->
      let cstrs = separate_map (break 1) ~f:pp_constraint cstr cstrs in
      let kw = Token.pp ~after:decl ~before:cstrs CONSTRAINT in
      prefix ~indent:2 ~spaces:1 decl
        (kw ^/^ hang 2 (break 0 ^^ cstrs))

  type keyword =
    | With_type of document
    | Type of rec_flag
    | And

  let pp
      ?override_name ?binder ~keyword
      { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
        ptype_manifest; ptype_ext_attributes = (extension, attrs);
        ptype_attributes; ptype_loc }
  =
    let name = Option.value override_name ~default:(str ptype_name) in
    let lhs = with_params ~decl_loc:ptype_loc ptype_params name in
    let manifest_opt = Option.map Core_type.pp ptype_manifest in
    let rhs =
      (* I didn't know how to express this nightmare more cleanly. *)
      match manifest_opt, ptype_private, ptype_kind with
      | None, None, Ptype_abstract -> None
      | Some manifest, None, Ptype_abstract -> Some manifest
      | Some manifest, Some loc, Ptype_abstract ->
        Some (group (string ~loc "private" ^/^ manifest))
      | Some manifest, None, kind ->
        let kind = non_abstract_kind kind in
        let equals = Token.pp ~after:manifest ~before:kind EQUAL in
        Some (manifest ^/^ equals ^/^ kind)
      | Some manifest, Some loc, kind ->
        let private_ = string ~loc "private" in
        let equals = Token.pp ~after:manifest ~before:private_ EQUAL in
        Some (manifest ^/^ equals ^/^ private_ ^/^ non_abstract_kind kind)
      | None, Some loc, kind ->
        assert (kind <> Ptype_abstract);
        let private_ = string ~loc "private" in
        Some (private_ ^/^ non_abstract_kind kind)
      | None, None, kind ->
        assert (kind <> Ptype_abstract);
        Some (non_abstract_kind kind)
    in
    let keyword =
      let pp_tok tok = Token.pp ~inside:ptype_loc ~before:lhs tok in
      match keyword with
      | With_type previous_tok ->
        let tok =
          Keyword.decorate (Token.pp ~after:previous_tok ~before:lhs TYPE)
            ~extension attrs
        in
        group (previous_tok ^/^ tok)
      | And -> Keyword.decorate (pp_tok AND) ~extension attrs
      | Type rf ->
        let tok = Keyword.decorate (pp_tok TYPE) ~extension attrs in
        match rf with
        | Recursive -> tok
        | Nonrecursive -> group (tok ^/^ Token.pp ~after:tok ~before:lhs NONREC)
    in
    match rhs with
    | Some rhs ->
      let rhs = add_constraints rhs ptype_cstrs in
      let binding = Binding.pp_simple ?binder ~keyword lhs rhs in
      Attribute.attach_to_top_item binding ptype_attributes
    | None ->
      let decl = prefix ~indent:2 ~spaces:1 keyword lhs in
      let decl = add_constraints decl ptype_cstrs in
      Attribute.attach_to_top_item decl ptype_attributes

  let pp_with_constraint ?override_name ?binder ~keyword td =
    pp ?override_name ?binder ~keyword td

  let pp_decl ~ext_attrs:(extension, attrs) rf decls =
    assert (extension = None);
    assert (attrs = []);
    let decls =
      let i = ref 0 in
      List.concat_map (fun decl ->
        let text, decl =
          let text, attrs =
            Attribute.extract_text decl.ptype_attributes
              ~item_start_pos:decl.ptype_loc.loc_start
          in
          text, { decl with  ptype_attributes = attrs }
        in
        let keyword = if !i = 0 then Type rf else And in
        incr i;
        let binding = pp ~keyword decl in
        Attribute.prepend_text text binding
      ) decls
    in
    separate (twice hardline) (List.hd decls) (List.tl decls)

  let pp_subst ~ext_attrs:(extension, attrs) decls =
    assert (extension = None);
    assert (attrs = []);
    let decls =
      let i = ref 0 in
      List.concat_map (fun decl ->
        let text, decl =
          let text, attrs =
            Attribute.extract_text decl.ptype_attributes
              ~item_start_pos:decl.ptype_loc.loc_start
          in
          text, { decl with  ptype_attributes = attrs }
        in
        let keyword = if !i = 0 then Type Recursive else And in
        incr i;
        let binding = pp ~binder:COLONEQUAL ~keyword decl in
        Attribute.prepend_text text binding
      ) decls
    in
    separate hardline (List.hd decls) (List.tl decls)
end


and Class_type : sig
  val pp : class_type -> document
  val pp_constr : loc:Location.t -> Longident.t -> core_type list -> document
end = struct
  let pp_constr ~loc name args =
    let name = Longident.pp name in
    match args with
    | [] -> name
    | x :: xs ->
      let after =
        let rec ends_in_obj = function
          | [] -> assert false
          | [ x ] -> Core_type.ends_in_obj x
          | _ :: xs -> ends_in_obj xs
        in
        if ends_in_obj args then break 1 else empty
      in
      let before =
        if Core_type.starts_with_obj x then break 1 else empty
      in
      let args =
        brackets ~loc (before ^^ tuple_fields Core_type.pp x xs ^^ after)
      in
      group args ^/^ name

  let rec pp { pcty_desc; pcty_loc; pcty_attributes } =
    let doc, attrs = pp_desc pcty_loc pcty_desc pcty_attributes in
    Attribute.attach_to_item doc attrs

  and pp_open ~loc od ct attrs =
    let od = Open_description.pp ~ext_attrs:(None, []) ~extra_attrs:attrs od in
    let ct = pp ct in
    let in_ = Token.pp ~after:od ~before:ct IN in
    let let_ = Token.pp ~inside:loc ~before:od LET in
    group (let_ ^/^ od ^/^ in_) ^/^ ct

  and pp_arrow lbl ct cty =
    let param = Core_type.pp_param (lbl, ct) in
    let cty = pp cty in
    let arrow = Token.pp ~after:param ~before:cty MINUSGREATER in
    param ^/^ group (arrow ^/^ cty)

  and pp_desc loc desc attrs =
    match desc with
    | Pcty_constr (ct, args) -> pp_constr ~loc ct args, attrs
    | Pcty_signature sg -> Class_signature.pp ~loc sg, attrs
    | Pcty_arrow (lbl, ct, cty) -> pp_arrow lbl ct cty, attrs
    | Pcty_extension ext -> Attribute.Extension.pp ~loc Item ext, attrs
    | Pcty_open (od, ct) -> pp_open ~loc od ct attrs, []
end


and Class_expr : sig
  val pp : class_expr -> document
end = struct
(* TODO: much of this is just copy pasted from Expression; factorize. *)
  let rec pp { pcl_desc; pcl_loc; pcl_attributes } =
    let desc, attrs = pp_desc ~loc:pcl_loc pcl_desc pcl_attributes in
    Attribute.attach_to_item (group desc) attrs

  and pp_fun ~loc ~attrs params ce =
    let params =
      separate_map (break 1) ~f:Fun_param.pp (List.hd params)
        (List.tl params)
    in
    let body = pp ce in
    (* FIXME: copied from expressions. factorize. *)
    let fun_ =
      let token = Token.pp ~inside:loc ~before:params FUN in
      Keyword.decorate token ~extension:None attrs
    in
    let arrow = Token.pp ~after:params ~before:body MINUSGREATER in
    prefix ~indent:2 ~spaces:1
      (group ((prefix ~indent:2 ~spaces:1 fun_ params) ^/^ arrow)) body

  and pp_apply ce = function
    | [] -> assert false (* can't apply without args! *)
    | arg :: args ->
      let ce = pp ce in
      Application.pp_simple ce arg args

  and pp_let ~loc rf vbs ce =
    let vbs =
      let previous_vb = ref None in
      List.concat_map (fun vb ->
        let text, vb =
          let text, attrs =
            Attribute.extract_text vb.pvb_attributes
              ~item_start_pos:vb.pvb_loc.loc_start
          in
          text, { vb with  pvb_attributes = attrs }
        in
        let binding = Value_binding.pp Attached_to_item vb in
        let keyword =
          let lhs = binding.lhs in
          let attrs =
            match vb.pvb_ext_attributes with
            | Some _, _ -> assert false
            | None, attrs -> attrs
          in
          let token, modifier =
            match !previous_vb with
            | None ->
              Token.pp ~inside:loc ~before:lhs LET,
              rec_token ~recursive_by_default:false rf
            | Some prev_vb -> Token.pp ~after:prev_vb ~before:lhs AND, None
          in
          let kw = Keyword.decorate token ~extension:None attrs in
          match modifier with
          | None -> kw
          | Some tok ->
            let modif = Token.pp ~after:kw ~before:lhs tok in
            kw ^/^ modif
        in
        let binding = Binding.pp ~keyword binding in
        previous_vb := Some binding;
        Attribute.prepend_text text binding
      ) vbs
    in
    let vbs = separate hardline (List.hd vbs) (List.tl vbs) in
    let ce = pp ce in
    let in_ = Token.pp ~after:vbs ~before:ce IN in
    group (vbs ^/^ in_) ^^ hardline ^^ ce

  and pp_constraint ~loc ce ct =
    let ce = pp ce in
    let ct = Class_type.pp ct in
    let colon = Token.pp ~after:ce ~before:ct COLON in
    group (parens ~loc (ce ^/^ colon ^/^ ct))

  and pp_open ~loc od ce attrs =
    let od = Open_description.pp ~ext_attrs:(None, []) ~extra_attrs:attrs od in
    let ce = pp ce in
    let in_ = Token.pp ~after:od ~before:ce IN in
    let let_ = Token.pp ~inside:loc ~before:od LET in
    group (let_ ^/^ od ^/^ in_) ^/^ ce

  and pp_desc ~loc desc attrs =
    match desc with
    | Pcl_constr (name, args) -> Class_type.pp_constr ~loc name args, attrs
    | Pcl_structure str -> Class_structure.pp ~loc str, attrs
    | Pcl_fun (params, ce) -> pp_fun ~loc ~attrs params ce, []
    | Pcl_apply (ce, args) -> pp_apply ce args, attrs
    | Pcl_let (rf, vbs, ce) -> pp_let ~loc rf vbs ce, attrs
    | Pcl_constraint (ce, ct) -> pp_constraint ~loc ce ct, attrs
    | Pcl_extension ext -> Attribute.Extension.pp ~loc Item ext, attrs
    | Pcl_open (od, ce) -> pp_open ~loc od ce attrs, []
    | Pcl_parens ce -> parens ~loc (pp ce), attrs
end


and Class_structure : sig
  val pp
    :  loc:Location.t
    -> ?ext_attrs:string loc option * attributes
    -> class_structure
    -> document

  val pp_constraint
    :  loc:Location.t
    -> ext_attrs:string loc option * attributes
    -> core_type
    -> core_type
    -> document
end = struct
  let pp_inherit ~loc ~ext_attrs:(extension, attrs) override ce alias =
    let pre =
      let ce = Class_expr.pp ce in
      let inh_kw = Token.pp ~inside:loc ~before:ce INHERIT in
      let inh_kw = 
        match override with
        | Override ->
          let bang = Token.pp ~after:inh_kw ~before:ce BANG in
          inh_kw ^^ bang
        | _ -> inh_kw
      in
      let inh_kw = Keyword.decorate inh_kw ~extension attrs in
      group (inh_kw ^/^ ce)
    in
    match alias with
    | None -> pre
    | Some name ->
      let name = str name in
      let as_ = Token.pp ~after:pre ~before:name AS in
      group (pre ^/^ as_ ^/^ name)

  let pp_virtual ~loc ~ext_attrs:(ext, attrs) kind name decorate_keyword ct =
    let name = str name in
    let ct = Core_type.pp ct in
    let keyword =
      let kw = Token.pp ~inside:loc ~before:name kind in
      let kw = Keyword.decorate kw ~extension:ext attrs in
      group (decorate_keyword kw name)
    in
    Binding.pp_simple ~binder:COLON ~keyword name ct

  let pp_concrete ~loc ~ext_attrs:(ext, attrs) kind name decorate_keyword
      override params (constr, coerce) expr =
    let name = str name in
    let keyword =
      let kw = Token.pp ~inside:loc ~before:name kind in
      let with_bang =
        match override with
        | Override -> kw ^^ Token.pp ~after:kw ~before:name BANG
        | _ -> kw
      in
      let kw = Keyword.decorate with_bang ~extension:ext attrs in
      group (decorate_keyword kw name)
    in
    let params = List.map Fun_param.pp params in
    let constr = Option.map Core_type.pp constr in
    let coerce = Option.map Core_type.pp coerce in
    let rhs = Binding.Rhs.Regular (Expression.pp expr) in
    Binding.pp ~keyword { lhs = name; params; constr; coerce; rhs }

  let pp_field_kind ~loc ~ext_attrs kind name decorate_keyword = function
    | Cfk_virtual ct -> pp_virtual ~loc ~ext_attrs kind name decorate_keyword ct
    | Cfk_concrete (override, params, cts, expr) ->
      pp_concrete ~loc ~ext_attrs kind name decorate_keyword override params cts 
        expr

  let pp_val ~loc ~ext_attrs name mut_virt cfk =
    let decorate_val keyword name =
      match mut_virt with
      | MV_none -> keyword
      | MV_virtual -> keyword ^/^ Token.pp ~after:keyword ~before:name VIRTUAL
      | MV_mutable _ -> keyword ^/^ Token.pp ~after:keyword ~before:name MUTABLE
      | MV_mut_virt ->
        let mut = Token.pp ~after:keyword ~before:name MUTABLE in
        let virt = Token.pp ~after:mut ~before:name VIRTUAL in
        keyword ^/^ mut ^/^ virt
      | MV_virt_mut ->
        let virt = Token.pp ~after:keyword ~before:name VIRTUAL in
        let mut = Token.pp ~after:virt ~before:name MUTABLE in
        keyword ^/^ virt ^/^ mut
    in
    pp_field_kind ~loc ~ext_attrs VAL name decorate_val cfk

  let pp_method ~loc ~ext_attrs name priv_virt cfk =
    let decorate_method keyword name =
      match priv_virt with
      | PV_none -> keyword
      | PV_virtual -> keyword ^/^ Token.pp ~after:keyword ~before:name VIRTUAL
      | PV_private -> keyword ^/^ Token.pp ~after:keyword ~before:name PRIVATE
      | PV_priv_virt ->
        let priv = Token.pp ~after:keyword ~before:name PRIVATE in
        let virt = Token.pp ~after:priv ~before:name VIRTUAL in
        keyword ^/^ priv ^/^ virt
      | PV_virt_priv ->
        let virt = Token.pp ~after:keyword ~before:name VIRTUAL in
        let priv = Token.pp ~after:virt ~before:name PRIVATE in
        keyword ^/^ virt ^/^ priv
    in
    pp_field_kind ~loc ~ext_attrs METHOD name decorate_method cfk

  let pp_constraint ~loc ~ext_attrs:(extension, attrs) ct1 ct2 =
    let ct1 = Core_type.pp ct1 in
    let ct2 = Core_type.pp ct2 in
    let keyword = Token.pp ~inside:loc ~before:ct1 CONSTRAINT in
    let keyword = Keyword.decorate keyword ~extension attrs in
    Binding.pp_simple ~keyword ct1 ct2

  let pp_init ~loc ~ext_attrs:(extension, attrs) expr =
    let expr = Expression.pp expr in
    let init = Token.pp ~inside:loc ~before:expr INITIALIZER in
    let init = Keyword.decorate init ~extension attrs in
    group (init ^/^ expr)

  let pp_field_desc ~loc ~ext_attrs = function
    | Pcf_inherit (override, ce, alias) -> pp_inherit ~loc ~ext_attrs override ce alias
    | Pcf_val (name, mut, cf) -> pp_val ~loc ~ext_attrs name mut cf
    | Pcf_method (name, priv, cf) -> pp_method ~loc ~ext_attrs name priv cf
    | Pcf_constraint (ct1, ct2) -> pp_constraint ~loc ~ext_attrs ct1 ct2
    | Pcf_initializer e -> pp_init ~loc ~ext_attrs e
    | Pcf_attribute attr -> Attribute.pp Free_floating attr
    | Pcf_extension ext -> Attribute.Extension.pp ~loc Structure_item ext

  let pp_field { pcf_desc; pcf_loc; pcf_ext_attributes; pcf_attributes } =
    let doc = pp_field_desc ~loc:pcf_loc ~ext_attrs:pcf_ext_attributes pcf_desc in
    Attribute.attach_to_top_item doc pcf_attributes

  let pp
      ~(loc:Location.t) ?ext_attrs:(extension, attrs=None, [])
      { pcstr_self; pcstr_fields }
  =
    let obj_with_self =
      match pcstr_self with
      | None ->
        let kw = Token.pp ~inside:loc OBJECT in
        Keyword.decorate kw ~extension attrs
      | Some pcstr_self ->
        let self = Pattern.pp pcstr_self in
        let obj =
          Keyword.decorate (Token.pp ~inside:loc ~before:self OBJECT) ~extension
            attrs
        in
        group (obj ^/^ self)
    in
    match pcstr_fields with
    | [] ->
      let end_ = Token.pp ~inside:loc ~after:obj_with_self END in
      obj_with_self ^/^ end_
    | f :: fs ->
      let fields = separate_map (twice hardline) ~f:pp_field f fs in
      let end_ = Token.pp ~inside:loc ~after:fields END in
      group (obj_with_self ^^ (nest 2 (break 1 ^^ fields)) ^/^ end_)
end


and Class_signature : sig
  val pp : loc:Location.t -> class_signature -> document
end = struct
  let pp_inherit ~loc ~ext_attrs:(extension, attrs) ct =
    let ct = Class_type.pp ct in
    let inh_kw = Token.pp ~inside:loc ~before:ct INHERIT in
    let inh_kw = Keyword.decorate ~extension inh_kw attrs in
    group (inh_kw ^/^ ct)

  let pp_val ~loc ~ext_attrs:(extension, attrs) (name, mut_virt, ct) =
    let name = str name in
    let ct = Core_type.pp ct in
    let keyword =
      let kw = Token.pp ~inside:loc ~before:name VAL in
      let kw = Keyword.decorate ~extension kw attrs in
      group
        (match mut_virt with
        | MV_none -> kw
        | MV_mutable _ ->
          let tok = Token.pp ~after:kw ~before:name MUTABLE in
          kw ^/^ tok
        | MV_virtual ->
          let virt = Token.pp ~after:kw ~before:name VIRTUAL in
          kw ^/^ virt
        | MV_mut_virt ->
          let mut = Token.pp ~after:kw ~before:name MUTABLE in
          let virt = Token.pp ~after:mut ~before:name VIRTUAL in
          kw ^/^ mut ^/^ virt
        | MV_virt_mut ->
          let virt = Token.pp ~after:kw ~before:name VIRTUAL in
          let mut = Token.pp ~after:virt ~before:name MUTABLE in
          kw ^/^ virt ^/^ mut)
    in
    Binding.pp_simple ~binder:COLON ~keyword name ct

  let pp_method ~loc ~ext_attrs:(extension, attrs) (name, priv_virt, ct) =
    let name = str name in
    let ct = Core_type.pp ct in
    let keyword =
      let kw = Token.pp ~inside:loc ~before:name METHOD in
      let kw = Keyword.decorate ~extension kw attrs in
      group
        (match priv_virt with
        | PV_none -> kw
        | PV_private ->
          let tok = Token.pp ~after:kw ~before:name PRIVATE in
          kw ^/^ tok
        | PV_virtual ->
          let virt = Token.pp ~after:kw ~before:name VIRTUAL in
          kw ^/^ virt
        | PV_priv_virt ->
          let priv = Token.pp ~after:kw ~before:name PRIVATE in
          let virt = Token.pp ~after:priv ~before:name VIRTUAL in
          kw ^/^ priv ^/^ virt
        | PV_virt_priv ->
          let virt = Token.pp ~after:kw ~before:name VIRTUAL in
          let priv = Token.pp ~after:virt ~before:name PRIVATE in
          kw ^/^ virt ^/^ priv)
    in
    Binding.pp_simple ~binder:COLON ~keyword name ct

  let pp_field_desc ~loc ~ext_attrs = function
    | Pctf_inherit ct -> pp_inherit ~loc ~ext_attrs ct
    | Pctf_val val_ -> pp_val ~loc ~ext_attrs val_
    | Pctf_method meth -> pp_method ~loc ~ext_attrs meth
    | Pctf_constraint (ct1, ct2) ->
      Class_structure.pp_constraint ~loc ~ext_attrs ct1 ct2
    | Pctf_attribute attr -> Attribute.pp Free_floating attr
    | Pctf_extension ext -> Attribute.Extension.pp ~loc Structure_item ext

  let pp_field { pctf_desc; pctf_loc; pctf_ext_attributes; pctf_attributes } =
    let doc =
      pp_field_desc ~loc:pctf_loc ~ext_attrs:pctf_ext_attributes pctf_desc
    in
    Attribute.attach_to_top_item doc pctf_attributes

  let pp ~loc { pcsig_self; pcsig_fields } =
    match pcsig_fields with
    | [] ->
      begin match pcsig_self with
      | None -> Empty_delimited.pp ~loc [] OBJECT END
      | Some pcsig_self ->
        let self = Core_type.pp pcsig_self in
        let obj_ = Token.pp ~inside:loc ~before:self OBJECT in
        let end_ = Token.pp ~inside:loc ~after:self END in
        prefix ~indent:2 ~spaces:1 (group (obj_ ^/^ self)) end_
      end
    | f :: fs ->
      let fields = separate_map (twice hardline) ~f:pp_field f fs in
      let obj_ =
        let obj = Token.pp ~inside:loc ~before:fields OBJECT in
        match pcsig_self with
        | None -> obj
        | Some pcsig_self ->
          let self = Core_type.pp pcsig_self in
          group (obj ^/^ self)
      in
      let end_ = Token.pp ~inside:loc ~after:fields END in
      group (prefix ~indent:2 ~spaces:1 obj_ fields ^/^ end_)
end


and Class_declaration : sig
  val pp
    :  ext_attrs:string loc option * attributes
    -> class_declaration list
    -> document
end = struct
  let pp ~ext_attrs:(extension, attrs) cds =
    assert (extension = None);
    assert (attrs = []);
    let cds =
      let previous_cd = ref None in
      List.concat_map (fun cd ->
        let
          { pci_virt; pci_params; pci_name; pci_term_params; pci_type;
            pci_expr; pci_ext_attributes = (extension, attrs);
            pci_loc; pci_attributes } =
          cd
        in
        let text, pci_attributes =
          Attribute.extract_text pci_attributes
            ~item_start_pos:pci_loc.loc_start
        in
        let lhs =
          Type_declaration.with_params ~decl_loc:pci_loc ~always_enclosed:true
            ~enclosing:brackets pci_params (str pci_name)
        in
        let binding =
          {
            Binding.lhs;
            params = List.map Fun_param.pp pci_term_params;
            constr = Option.map Class_type.pp pci_type;
            coerce = None;
            rhs = Binding.Rhs.Regular (Class_expr.pp pci_expr)
          }
        in
        let keyword =
          match !previous_cd with
          | None -> Token.pp ~inside:pci_loc ~before:lhs CLASS
          | Some cd -> Token.pp ~after:cd ~before:lhs AND
        in
        let keyword = Keyword.decorate keyword ~extension attrs in
        let keyword =
          match pci_virt with
          | Concrete -> keyword
          | Virtual ->
            let virt = Token.pp ~after:keyword ~before:lhs VIRTUAL in
            group (keyword ^/^ virt)
        in
        let doc = Binding.pp binding ~keyword in
        previous_cd := Some doc;
        Attribute.prepend_text text @@
          Attribute.attach_to_top_item doc pci_attributes
      ) cds
    in
    separate (twice hardline) (List.hd cds) (List.tl cds)
end


and Class_description : sig
  val pp
    :  ext_attrs:string loc option * attributes
    -> class_description list
    -> document
end = struct
  let pp ~ext_attrs:(extension, attrs) cds =
    assert (extension = None);
    assert (attrs = []);
    let cds =
      let previous_cd = ref None in
      List.concat_map (fun cd ->
        let
          { pci_virt; pci_params; pci_name; pci_term_params; pci_type;
            pci_expr; pci_ext_attributes = (extension, attrs);
            pci_loc; pci_attributes } =
          cd
        in
        let text, pci_attributes =
          Attribute.extract_text pci_attributes
            ~item_start_pos:pci_loc.loc_start
        in
        let lhs =
          Type_declaration.with_params ~always_enclosed:true ~enclosing:brackets
            ~decl_loc:pci_loc pci_params (str pci_name)
        in
        assert (Option.is_none pci_type);
        let binding =
          {
            Binding.lhs;
            params = List.map Fun_param.pp pci_term_params;
            constr = None;
            coerce = None;
            rhs = Binding.Rhs.Regular (Class_type.pp pci_expr)
          }
        in
        let keyword =
          match !previous_cd with
          | None -> Token.pp ~inside:pci_loc ~before:lhs CLASS
          | Some cd -> Token.pp ~after:cd ~before:lhs AND
        in
        let keyword = Keyword.decorate keyword ~extension attrs in
        let keyword =
          match pci_virt with
          | Concrete -> keyword
          | Virtual ->
            let virt = Token.pp ~after:keyword ~before:lhs VIRTUAL in
            group (keyword ^/^ virt)
        in
        let doc = Binding.pp binding ~binder:COLON ~keyword in
        previous_cd := Some doc;
        Attribute.prepend_text text @@
          Attribute.attach_to_top_item doc pci_attributes
      ) cds
    in
    separate (twice hardline) (List.hd cds) (List.tl cds)
end


and Class_type_declaration : sig
  val pp
    :  ext_attrs:string loc option * attributes
    -> class_description list
    -> document
end = struct
  let pp ~ext_attrs:(extension, attrs) cds =
    assert (extension = None);
    assert (attrs = []);
    let cds =
      let previous_cd = ref None in
      List.concat_map (fun cd ->
        let
          { pci_virt; pci_params; pci_name; pci_term_params; pci_type;
            pci_expr; pci_ext_attributes = (extension, attrs);
            pci_loc; pci_attributes } =
          cd
        in
        let text, pci_attributes =
          Attribute.extract_text pci_attributes
            ~item_start_pos:pci_loc.loc_start
        in
        let lhs =
          Type_declaration.with_params ~always_enclosed:true ~enclosing:brackets
            ~decl_loc:pci_loc pci_params (str pci_name)
        in
        assert (Option.is_none pci_type);
        let binding =
          {
            Binding.lhs;
            params = List.map Fun_param.pp pci_term_params;
            constr = None;
            coerce = None;
            rhs = Binding.Rhs.Regular (Class_type.pp pci_expr)
          }
        in
        let keyword =
          match !previous_cd with
          | Some cd -> Token.pp ~after:cd ~before:lhs AND
          | None ->
            let class_ = Token.pp ~inside:pci_loc ~before:lhs CLASS in
            let type_ = Token.pp ~after:class_ ~before:lhs TYPE in
            group (class_ ^/^ type_)
        in
        let keyword = Keyword.decorate keyword ~extension attrs in
        let keyword =
          match pci_virt with
          | Concrete -> keyword
          | Virtual ->
            let virt = Token.pp ~after:keyword ~before:lhs VIRTUAL in
            group (keyword ^/^ virt)
        in
        let doc = Binding.pp binding ~keyword in
        previous_cd := Some doc;
        Attribute.prepend_text text @@
          Attribute.attach_to_top_item doc pci_attributes
      ) cds
    in
    separate (twice hardline) (List.hd cds) (List.tl cds)
end


and Open_description : sig
  val pp
    :  ?extra_attrs:attributes
    -> ext_attrs:string loc option * attributes
    -> open_description
    -> document
end = struct
  let pp
      ?(extra_attrs=[]) ~ext_attrs:(extension, attrs)
      { popen_expr; popen_override; popen_attributes; popen_loc }
  =
    let expr = Longident.pp popen_expr in
    let kw =
      let tok = Token.pp ~inside:popen_loc ~before:expr OPEN in
      match popen_override with
      | Override ->
        let over = Token.pp ~after:tok ~before:expr BANG in
        tok ^^ over
      | _ -> tok
    in
    let kw = Keyword.decorate kw ~extension attrs in
    let opn = group (Attribute.attach_to_item kw extra_attrs ^/^ expr) in
    Attribute.attach_to_top_item opn popen_attributes
end


and Open_declaration : sig
  val pp
    :  ?ext_attrs:string loc option * attributes
    -> Attribute.kind
    -> open_declaration
    -> document
end = struct
  let pp
      ?ext_attrs:(extension, attrs=None, []) kind
      { popen_expr; popen_override; popen_attributes; popen_loc }
  =
    let expr = Module_expr.pp popen_expr in
    let kw =
      let tok = Token.pp ~inside:popen_loc ~before:expr OPEN in
      match popen_override with
      | Override ->
        let over = Token.pp ~after:tok ~before:expr BANG in
        tok ^^ over
      | _ -> tok
    in
    let kw = Keyword.decorate kw ~extension attrs in
    let opn = group (kw ^/^ expr) in
    Attribute.attach kind opn popen_attributes
end


let interface sg =
  let doc =
    match sg with
    | [] -> empty
    | si :: sg -> Signature.pp_nonempty si sg
  in
  Document.attach_surrounding_comments doc

let implementation str =
  let doc =
    match str with
    | [] -> empty
    | si :: st -> Structure.pp_nonempty si st
  in
  Document.attach_surrounding_comments doc
