open Source_parsing
open Asttypes
open Source_tree

open Document
open struct type document = Document.t end

let module_name { txt; loc} =
  match txt with
  | None -> underscore ~loc
  | Some name -> string ~loc name

let rec_token ~recursive_by_default rf : Source_parsing.Parser.token option =
  match rf, recursive_by_default with
  | Recursive, false   -> Some REC
  | Nonrecursive, true -> Some NONREC
  | _, _ -> None

module Longident : sig
  include module type of struct include Longident end

  val pp : t -> document
  val pp_ident : string loc -> document
end = struct
  include Longident

  let pp_ident s =
    match Ident_class.classify s with
    | Normal -> str s
    | Infix_op { loc; txt } when txt <> "" && String.get txt 0 = '*' ->
      parens (string ~loc (" " ^ txt ^ " "))
    | Infix_op _ | Prefix_op _ -> parens (str s)

  let rec pp lid =
    group (aux lid)

  and aux = function
    | Lident s -> pp_ident s
    | Ldot (lid, s) -> concat (pp lid) ~sep:PPrint.(dot ^^ break 0) (str s)
    | Lapply (l1, l2) -> concat (pp l1) ~sep:(break 0) (parens (pp l2))

  let pp lid = hang 2 (pp lid)

  let () = Constructor_decl.pp_longident := pp
end

module Constant : sig
  val pp : loc:Location.t -> constant -> document

  (* Helpers. *)
  val pp_string_lit : loc:Location.t -> string -> document
  val pp_quoted_string : loc:Location.t -> delim:string -> string -> document
end = struct
  let pp_string_lit ~loc s = quoted_string ~loc (String.escaped s)
  let pp_quoted_string ~loc ~delim s =
    let delim = PPrint.string delim in
    braces (
      enclose ~before:PPrint.(delim ^^ bar) ~after:PPrint.(bar ^^ delim)
        (quoted_string ~loc s)
    )

  let pp ~loc = function
    | Pconst_float (nb, suffix_opt)
    | Pconst_integer (nb, suffix_opt) ->
      let nb =
        match suffix_opt with
        | None -> nb
        | Some s -> nb ^ (String.make 1 s)
      in
      (* FIXME? nb might start with a minus… which might implying parenthesing
         is required in some contexts. *)
      let doc = string ~loc nb in
      if String.get nb 0 = '-' then parens doc else doc
    | Pconst_char c ->
      let c = Char.escaped c in
      squotes (string ~loc c)
    | Pconst_string (_, None) ->
      let s = Source_parsing.Source.source_between loc.loc_start loc.loc_end in
      quoted_string ~adjust_indent:true ~loc s
    | Pconst_string (_, Some _)   ->
      let s = Source_parsing.Source.source_between loc.loc_start loc.loc_end in
      quoted_string ~loc s
end

module Polymorphic_variant_tag : sig
  val pp : label loc -> document
end = struct
  let pp tag = string ~loc:tag.loc ("`" ^ tag.txt)
end

module rec Attribute : sig
  type kind =
    | Free_floating
    | Attached_to_structure_item
    | Attached_to_item

  val has_non_doc : attributes -> bool

  val pp : kind -> attribute -> document

  val attach : ?spaces:int -> kind -> document -> attributes -> document
  val attach_to_item : ?spaces:int -> document -> attributes -> document
  val attach_to_top_item : document -> attributes -> document

  val extract_text : item_start_pos:Lexing.position -> attributes -> attributes * attributes
  val prepend_text : attributes -> document -> document list
end = struct
  type kind =
    | Free_floating
    | Attached_to_structure_item
    | Attached_to_item

  let ats kind =
    match kind with
    | Free_floating -> "@@@"
    | Attached_to_structure_item -> "@@"
    | Attached_to_item -> "@"

  let pp_attr kind attr_name attr_payload =
    let tag = string ~loc:attr_name.loc (ats kind ^ attr_name.txt) in
    group (brackets (Payload.pp_after ~tag attr_payload))

  (* :/ *)
  let pp_doc ~loc = function
    | PStr [
        { pstr_desc =
            Pstr_eval ({ pexp_desc =
                           Pexp_constant Pconst_string (s, None); pexp_loc; _}, []); _ }
      ] ->
      let doc = docstring s pexp_loc in
      Location.mkloc doc loc
    | _ -> assert false

  let pp kind { attr_name; attr_payload; attr_loc } =
    match attr_name.txt with
    | "ocaml.doc" ->
      assert (kind <> Free_floating);
      pp_doc attr_payload ~loc:attr_loc
    | "ocaml.text" ->
      (*
         The following is not true in cases like:
         {[
           type a = Foo | Bar

           (** Haha! *)

           and b = { x : int }
         ]}

         TODO? handle docstring before the call to [pp], i.e. directly in
         [attach]. So in cases like this… we don't attach (that is: we don't
         indent)?

      assert (kind = Free_floating);
      *)
      pp_doc attr_payload ~loc:attr_loc
    | _ ->
      pp_attr kind attr_name attr_payload

  let attach ?(spaces=1) kind doc attrs =
    let postdoc, attrs =
      match
        List.partition (fun { attr_name; _ } -> attr_name.txt = "ocaml.doc")
          attrs
      with
      | [], attrs -> None, attrs
      | docs, attrs ->
        let rev_docs = List.rev docs in
        let doc = List.hd rev_docs in
        Some doc, List.rev_append (List.tl rev_docs) attrs
    in
    let with_attrs =
      match attrs with
      | [] -> doc
      | attr :: attrs ->
        group (
          prefix ~indent:2 ~spaces doc
            (separate_map (PPrint.break 0) ~f:(pp kind) attr attrs)
        )
    in
    match postdoc with
    | None -> with_attrs
    | Some { attr_payload; attr_loc; _ } ->
      let doc =
        prefix ~indent:0 ~spaces
          with_attrs
          (pp_doc attr_payload ~loc:attr_loc)
      in
      if kind = Attached_to_structure_item && requirement doc.txt < 80 (* Ugly! *)
      then
        break_after ~spaces:0 doc
      else
        doc

  let has_non_doc =
    List.exists (fun attr ->
      match attr.attr_name.txt with
      | "ocaml.doc" | "ocaml.text" -> false
      | _ -> true
    )

  let attach_to_item ?spaces doc =
    attach ?spaces Attached_to_item doc

  let () = Constructor_decl.attach_attributes := attach_to_item
  let () = Polymorphic_variant.attach_attributes := attach_to_item

  let attach_to_top_item doc =
    attach Attached_to_structure_item doc

  let extract_text ~item_start_pos =
    let rec aux acc = function
      | { attr_name = { txt = "ocaml.text" | "ocaml.doc"; _ };
          attr_loc; _ } as attr
        :: attrs
        when Source_parsing.Comments.compare_pos
            attr_loc.loc_start item_start_pos <= 0 ->
        aux (attr :: acc) attrs
      | attrs -> List.rev acc, attrs
    in
    aux []

  let prepend_text attrs doc =
    let text, docstring =
      List.partition (fun { attr_name; _ } -> attr_name.txt = "ocaml.text")
        attrs
    in
    let doc =
      match docstring with
      | [] -> doc
      | [ { attr_loc = loc; attr_payload; _} ] ->
        concat ~sep:hardline (pp_doc ~loc attr_payload) doc
      | _ -> assert false
    in
    match text with
    | [] -> [ doc ]
    | text :: texts ->
      let texts =
        separate_map PPrint.hardline ~f:(fun { attr_payload; attr_loc; _} ->
            pp_doc ~loc:attr_loc attr_payload)
          text texts
      in
      [ texts; doc ]
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

  let percents = function
    | Structure_item -> "%%"
    | Item -> "%"

  let pp kind ({ Location.txt = ext_name; loc }, ext_payload) =
    let tag = string ~loc (percents kind ^ ext_name) in
    brackets (Payload.pp_after ~tag ext_payload)
end

and Keyword : sig
  val decorate :
    document -> extension:string loc option -> attributes -> later:_ loc ->
    document
end = struct
  let decorate token ~extension attrs ~later =
    let kw =
      match extension with
      | None -> token
      | Some ext ->
        let percent = token_between token later PERCENT in
        token ^^ percent ^^ str ext
    in
    Attribute.attach_to_item ~spaces:0 kw attrs
end

and Payload : sig
  val pp_after : tag:document -> payload -> document
end = struct
  let pstr tag = function
    | [] -> tag
    | si :: st ->
      let st = Structure.pp_nonempty si st in
      tag ^^ nest 2 (break_before st)

  let psig tag = function
    | [] -> tag
    | si :: sg ->
      let sg = Signature.pp_nonempty si sg in
      let colon = token_between tag sg COLON in
      tag ^^ nest 2 (colon ^/^ sg)

  let ptyp tag ct =
    let break_after =
      if Core_type.ends_in_obj ct
      then break_after ~spaces:1
      else (fun x -> x)
    in
    let ct = break_after (Core_type.pp [] ct) in
    let colon = token_between tag ct COLON in
    tag ^^ nest 2 (colon ^/^ ct)

  let ppat tag p =
    let p = Pattern.pp [] p in
    let qmark = token_between tag p QUESTION in
    tag ^^ nest 2 (qmark ^/^ p)

  let ppat_guard tag p e =
    let p = Pattern.pp [] p in
    let e = Expression.pp [] e in
    let qmark = token_between tag p QUESTION in
    let when_ = token_between p e WHEN in
    tag ^^ nest 2 (
      qmark ^/^ p ^/^
      group (when_ ^/^ e)
    )

  let pp_after ~tag = function
    | PStr st -> pstr tag st
    | PSig sg -> psig tag sg
    | PTyp ct -> ptyp tag ct
    | PPat (p, None) -> ppat tag p
    | PPat (p, Some e) -> ppat_guard tag p e
end

and Core_type : sig
  val ends_in_obj : core_type -> bool
  val starts_with_obj : core_type -> bool

  val pp : Printing_stack.t -> core_type -> document
  val pp_param : Printing_stack.t -> (arg_label * core_type) -> document
end = struct
  let rec starts_with_obj core_type =
    match core_type.ptyp_desc with
    | Ptyp_alias (lhs, _)
    | Ptyp_tuple (lhs :: _)
    | Ptyp_arrow ((_, lhs) :: _, _) -> starts_with_obj lhs
    | Ptyp_object (_, _) -> true
    | Ptyp_any
    | Ptyp_var _
    | Ptyp_tuple _
    | Ptyp_arrow _
    | Ptyp_constr (_, _)
    | Ptyp_class (_, _)
    | Ptyp_variant (_, _, _)
    | Ptyp_poly _
    | Ptype_poly _
    | Ptyp_package _
    | Ptyp_extension _ -> false

  let rec ends_in_obj core_type =
    match core_type.ptyp_desc with
    | Ptyp_arrow (_, rhs)
    | Ptype_poly (_, rhs)
    | Ptyp_poly (_, rhs) -> ends_in_obj rhs
    | Ptyp_tuple lst -> ends_in_obj (List.hd (List.rev lst))
    | Ptyp_object (_, _) -> true
    | Ptyp_any
    | Ptyp_var _
    | Ptyp_constr (_, _)
    | Ptyp_class (_, _)
    | Ptyp_alias (_, _)
    | Ptyp_variant (_, _, _)
    | Ptyp_package _
    | Ptyp_extension _ -> false

  let pp_var ~loc v =
    match String.index_opt v '\'' with
    | None -> string ~loc ("'" ^ v)
    | Some _ -> string ~loc ("' " ^ v)

  let rec pp ps { ptyp_loc; ptyp_desc; ptyp_attributes; ptyp_loc_stack = _ } =
    let ps, enclose =
      if Attribute.has_non_doc ptyp_attributes then
        Printing_stack.parenthesize (Attribute :: ps)
      else
        ps, Fun.id
    in
    let ps = Printing_stack.Core_type ptyp_desc :: ps in
    let doc = group (pp_desc ~loc:ptyp_loc ps ptyp_desc) in
    let doc = Attribute.attach_to_item doc ptyp_attributes in
    enclose doc

  and pp_desc ~loc ps = function
    | Ptyp_any -> underscore ~loc
    | Ptyp_var v -> pp_var ~loc v
    | Ptyp_arrow (params, ct2) -> pp_arrow ps params ct2
    | Ptyp_tuple lst -> pp_tuple ps lst
    | Ptyp_constr (name, args) -> pp_constr ps name args
    | Ptyp_object (fields, closed) -> pp_object ~loc fields closed
    | Ptyp_class (name, args) -> pp_class ps name args
    | Ptyp_alias (ct, alias) -> pp_alias ps ct alias
    | Ptyp_variant (fields, closed, present) ->
      Polymorphic_variant.pp_row ~loc fields closed present
    | Ptyp_poly (vars, ct) -> pp_poly vars ct
    | Ptype_poly (vars, ct) -> pp_newtype_poly ~loc vars ct
    | Ptyp_package pkg -> pp_package ~loc pkg
    | Ptyp_extension ext -> Extension.pp Item ext

  and pp_param ps (arg_label, ct) =
    let ct = hang 0 @@ pp ps ct in
    match arg_label with
    | Nolabel -> ct
    | Labelled l ->
      begin match token_before ~start:l.loc.loc_end ct COLON with
      | colon -> str l ^^ colon ^^ break_before ~spaces:0 ct
      | exception (Not_found | Assert_failure _ (* gloups *)) ->
        let lbl = string ~loc:l.loc (l.txt ^ ":") in
        lbl ^^ break_before ~spaces:0 ct
      end
    | Optional l ->
      match token_before ~start:l.loc.loc_end ct COLON with
      | colon ->
        (* Form with 3 tokens. Ideally, '?' would also be fetched and
           concatenated. *)
        let opt_label = string ~loc:l.loc ("?" ^ l.txt) in
        opt_label ^^ colon ^^ break_before ~spaces:0 ct
      | exception (Not_found | Assert_failure _ (* gloups. *))  ->
        let opt_label = string ~loc:l.loc ("?" ^ l.txt ^ ":") in
        opt_label ^^ break_before ~spaces:0 ct

  and pp_arrow ps params res =
    let ps, enclose = Printing_stack.parenthesize ps in
    let params =
      left_assoc_map ~sep:MINUSGREATER ~f:(pp_param ps) (List.hd params)
        (List.tl params)
    in
    let res = pp (List.tl ps) res in
    let arrow = token_between params res MINUSGREATER in
    let doc = params ^/^ group (arrow ^/^ res) in
    enclose doc

  and pp_tuple ps = function
    | [] -> assert false
    | x :: xs ->
      let ps, enclose = Printing_stack.parenthesize ps in
      let doc = left_assoc_map ~sep:STAR ~f:(pp ps) x xs in
      enclose doc

  and pp_constr ps name args =
    let name = Longident.pp name in
    match args with
    | [] -> name
    | x :: xs -> pp_params ps x xs ^/^ name

  and pp_params ps first = function
    | []   -> pp ps first
    | rest -> parens (separate_map comma ~f:(pp []) first rest)

  and pp_object ~loc fields closed =
    let fields = List.map Object_field.pp fields in
    let fields =
      match closed with
      | OClosed -> fields
      | OOpen loc -> fields @ [ string ~loc "..", [] ]
    in
    Record_like.pp ~loc ~formatting:Fit_or_vertical ~left:langle ~right:rangle
      fields

  and pp_class ps name args =
    let name = sharp ++ Longident.pp name in
    match args with
    | [] -> name
    | x :: xs -> pp_params ps x xs ^/^ name

  and pp_alias ps ct alias =
    let ps, enclose = Printing_stack.parenthesize ps in
    let ct = pp ps ct in
    let alias = pp_var ~loc:alias.loc alias.txt in
    let as_ = token_between ct alias AS in
    (* TODO: hang & ident one linebreak *)
    let doc = ct ^/^ as_ ^/^ alias in
    enclose doc

  and pp_poly vars ct =
    (* FIXME: doesn't look right. *)
    let ct = pp [] ct in
    match vars with
    | [] -> ct
    | v :: vs ->
      let vars= separate_map space ~f:(fun v -> pp_var ~loc:v.loc v.txt) v vs in
      let dot = token_between vars ct DOT in
      prefix ~indent:2 ~spaces:1
        (group (vars ^^ dot))
        ct

  and pp_newtype_poly ~loc vars ct =
    (* FIXME: doesn't look right. *)
    let ct = pp [] ct in
    match vars with
    | [] -> ct
    | v :: vs ->
      let type_ = token_before ~start:loc.loc_start v TYPE in
      let vars = separate_map space ~f:str v vs in
      let dot = token_between vars ct DOT in
      prefix ~indent:2 ~spaces:1
        (group (type_ ^/^ vars ^^ dot))
        ct

  and pp_package ~loc pkg =
    let module_ = string ~loc "module" in
    parens (module_ ^/^ Package_type.pp pkg)

  let () = Constructor_decl.pp_core_type := pp
  let () = Polymorphic_variant.pp_core_type := pp
end

and Object_field : sig
  val pp : object_field -> document * document list
end = struct
  let pp_otag name ct =
    let name = str name in
    let ct = Core_type.pp [] ct in
    let colon = token_between name ct COLON in
    group (name ^^ colon) ^/^ ct

  let pp_desc = function
    | Otag (name, ct) -> pp_otag name ct
    | Oinherit ct -> Core_type.pp [] ct

  let pp { pof_desc; pof_attributes; _ } =
    let desc = pp_desc pof_desc in
    desc, List.map (Attribute.pp Attached_to_item) pof_attributes
end

and Package_type : sig
  val pp : package_type -> document
end = struct
  let pp_constr (lid, ct) =
    let lid = Longident.pp lid in
    let ct = Core_type.pp [] ct in
    let colon = token_between lid ct EQUAL in
    lid ^/^ colon ^/^ ct

  let pp (lid, constrs) =
    let lid = Longident.pp lid in
    match constrs with
    | [] -> lid
    | x :: xs ->
      let constrs =
        separate_map
          PPrint.(break 1 ^^ !^"and" ^^ break 1 ^^ !^"type" ^^ break 1)
          ~f:pp_constr x xs
      in
      let sep = PPrint.(break 1 ^^ !^"with" ^/^ !^"type" ^^ break 1) in
      group (concat lid constrs ~sep)
end

and Pattern : sig
  val pp : ?indent:int -> Printing_stack.t -> pattern -> document
end = struct
  let rec pp ?indent ps { ppat_desc; ppat_attributes; ppat_loc; _ } =
    let ps, enclose =
      if Attribute.has_non_doc ppat_attributes then
        Printing_stack.parenthesize (Attribute :: ps)
      else
        ps, Fun.id
    in
    let ps = Printing_stack.Pattern ppat_desc :: ps in
    let desc = pp_desc ?indent ~loc:ppat_loc ps ppat_desc in
    let doc = Attribute.attach_to_item desc ppat_attributes in
    enclose doc

  and pp_alias ps pat alias =
    let ps, enclose = Printing_stack.parenthesize ps in
    let pat = pp ps pat in
    let alias = str alias in
    let as_ = token_between pat alias AS in
    let doc = pat ^^ group (nest 2 (break_before ~spaces:1 as_ ^/^ alias)) in
    enclose doc

  and pp_interval c1 c2 =
    let c1 = Constant.pp ~loc:c1.loc c1.txt in
    let c2 = Constant.pp ~loc:c2.loc c2.txt in
    let dotdot = token_between c1 c2 DOTDOT in
    c1 ^/^ dotdot ^/^ c2

  (* FIXME? nest on the outside, not in each of them. *)

  and pp_tuple ps lst =
    let ps, enclose = Printing_stack.parenthesize ps in
    let doc =
      group (
        separate_map PPrint.(comma ^^ break 1) ~f:(pp ps)
          (List.hd lst) (List.tl lst)
      )
    in
    enclose doc

  and pp_list_literal ~loc elts =
    let elts = List.map (pp []) elts in
    List_like.pp ~loc
      ~formatting:Wrap (* TODO: add an option *)
      ~left:lbracket ~right:rbracket
      elts

  and pp_cons ps hd tl =
    let ps = Printing_stack.top_is_op ~on_left:true "::" ps in
    let ps, enclose = Printing_stack.parenthesize ps in
    let hd = pp ps hd in
    let ps = Printing_stack.top_is_op ~on_left:false "::" ps in
    let tl = pp ps tl in
    let cons = token_between hd tl COLONCOLON in
    let doc = infix ~indent:2 ~spaces:1 cons hd tl in
    enclose doc

  and pp_construct ps name arg_opt =
    let name = Longident.pp name in
    match arg_opt with
    | None -> name
    | Some p ->
      let ps, enclose = Printing_stack.parenthesize ps in
      let doc = prefix ~indent:2 ~spaces:1 name (pp ps p) in
      enclose doc

  and pp_variant ps tag arg_opt =
    let tag = Polymorphic_variant_tag.pp tag in
    match arg_opt with
    | None -> tag
    | Some p ->
      let ps, enclose = Printing_stack.parenthesize ps in
      let arg = pp ps p in
      enclose (tag ^/^ arg)

  and pp_record_field ps (lid, ctyo, pato) =
    let params =
      let pos = (Longident.endpos lid).loc_end in
      { txt = []; loc = { loc_start = pos; loc_end = pos }}
    in
    let binding : Binding.t =
      { lhs = Longident.pp lid;
        params;
        constr = Option.map (Core_type.pp []) ctyo;
        coerce = None;
        rhs = Option.map (pp ps) pato;
      }
    in
    Binding.pp binding

  and pp_record ~loc ps pats closed =
    let _, enclose = Printing_stack.parenthesize ps in
    let fields = List.map (pp_record_field [ Record_field ]) pats in
    let extra_fields =
      match closed with
      | OClosed -> []
      | OOpen loc -> [underscore ~loc]
    in
    List_like.pp ~loc
      ~formatting:!Options.Record.pattern
      ~left:lbrace ~right:rbrace
      (fields @ extra_fields)
    |> enclose

  and pp_array ~loc ps pats =
    let pats = List.map (pp ps) pats in
    (* TODO: add an option *)
    List_like.pp ~loc ~formatting:Wrap
      ~left:PPrint.(lbracket ^^ bar) ~right:PPrint.(bar ^^ rbracket) pats

  and pp_or ~indent ps p1 p2 =
    let ps, enclose = Printing_stack.parenthesize ps in
    let p1 =
      let ps = Printing_stack.top_is_op ~on_left:true "|" ps in
      pp ~indent ps p1
    in
    let p2 = pp ~indent ps p2 in
    let pipe = token_between p1 p2 BAR in
    let or_ = p1 ^/^ group (pipe ^/^ p2) in
    enclose or_

  and pp_constraint p ct =
    let p = pp [] p in
    let ct = Core_type.pp [] ct in
    let colon = token_between p ct COLON in
    parens (p ^/^ colon ^/^ ct)

  and pp_type typ =
    sharp ++ Longident.pp typ

  and pp_lazy ~loc ps p =
    let ps, enclose = Printing_stack.parenthesize ps in
    let lazy_ = string ~loc "lazy" in
    enclose (lazy_ ^/^ pp ps p)

  and pp_unpack mod_name ct =
    let mod_name = module_name mod_name in
    let with_constraint =
      match ct with
      | None -> mod_name
      | Some pkg ->
        let constr = Package_type.pp pkg in
        let colon = token_between mod_name constr COLON in
        mod_name ^/^ colon ^/^ constr
    in
    enclose ~before:PPrint.(!^"(module ") ~after:PPrint.(!^")")
      with_constraint

  and pp_exception ~loc ps p =
    string ~loc "exception" ^/^ pp ps p

  and pp_open ps lid p =
    let lid = Longident.pp lid in
    let pat = pp ps p in
    let dot = token_between lid pat DOT in
    lid ^^ dot ^^ break_before ~spaces:0 pat

  and pp_var v = Longident.pp_ident v

  and pp_desc ?(indent=0) ~loc ps = function
    | Ppat_or (p1, p2) -> pp_or ~indent ps p1 p2
    | otherwise ->
      nest indent (
        match otherwise with
        | Ppat_or _ -> assert false
        | Ppat_any -> underscore ~loc
        | Ppat_var v -> pp_var v
        | Ppat_alias (pat, alias) -> pp_alias ps pat alias
        | Ppat_constant c -> Constant.pp ~loc c
        | Ppat_interval (c1, c2) -> pp_interval c1 c2
        | Ppat_tuple pats -> pp_tuple ps pats
        | Ppat_construct (name, arg) -> pp_construct ps name arg
        | Ppat_list_lit pats -> pp_list_literal ~loc pats
        | Ppat_cons (hd, tl) -> pp_cons ps hd tl
        | Ppat_variant (tag, arg) -> pp_variant ps tag arg
        | Ppat_record (pats, closed) -> pp_record ~loc ps pats closed
        | Ppat_array pats -> pp_array ~loc ps pats
        | Ppat_constraint (p, ct) -> pp_constraint p ct
        | Ppat_type pt -> pp_type pt
        | Ppat_lazy p -> pp_lazy ~loc ps p
        | Ppat_unpack (name, typ) -> pp_unpack name typ
        | Ppat_exception p -> pp_exception ~loc ps p
        | Ppat_extension ext -> Extension.pp Item ext
        | Ppat_open (lid, p) -> pp_open ps lid p
      )
end

and Application : sig
  val pp_simple : Printing_stack.t -> document
    -> (arg_label * expression) -> (arg_label * expression) list -> document

  val pp : Printing_stack.t -> expression -> (arg_label * expression) list
    -> document
end = struct
  let argument ps (lbl, exp) =
    let suffix ~prefix:sym lbl =
      match exp.pexp_desc with
      | Pexp_ident Lident id when lbl.txt = id.txt -> sym ++ str lbl
      | Pexp_fun (params, body) when exp.pexp_attributes = [] ->
        let lbl = string ~loc:lbl.loc (lbl.txt ^ ":") in
        let ps = Printing_stack.Expression exp.pexp_desc :: ps in
        let fun_, args, arrow, body =
          Expression.fun_chunks ~loc:exp.pexp_loc
            ~ext_attrs:exp.pexp_ext_attributes ps params body
        in
        let open_ = (sym ++ lbl) ^^ (lparen ++ break_before ~spaces:0 fun_) in
        let unclosed =
          prefix ~indent:2 ~spaces:1
            (group ((prefix ~indent:2 ~spaces:1 open_ args) ^/^ arrow))
            body
        in
        break_after ~spaces:0 unclosed +++ rparen
      | Pexp_function (c :: cs) when exp.pexp_attributes = [] ->
        let lbl = string ~loc:lbl.loc (lbl.txt ^ ":") in
        let ps = Printing_stack.Expression exp.pexp_desc :: ps in
        let function_, cases =
          Expression.function_chunks ~loc:exp.pexp_loc
            ~ext_attrs:exp.pexp_ext_attributes ps c cs
        in
        let open_ =
          (sym ++ lbl) ^^ (lparen ++ break_before ~spaces:0 function_)
        in
        let unclosed = open_ ^^ cases in
        break_after ~spaces:0 unclosed +++ rparen
      | _ ->
        let lbl = string ~loc:lbl.loc (lbl.txt ^ ":") in
        let exp = Expression.pp ps exp in
        group (sym ++ lbl ^^ break_before ~spaces:0 exp)
    in
    match lbl with
    | Nolabel -> Expression.pp ps exp
    | Labelled lbl -> suffix ~prefix:tilde lbl
    | Optional lbl -> suffix ~prefix:qmark lbl

  type argument =
    | Function of {
        fst_chunk: document;
        break: bool;
        snd_chunk: document;
      }
    | Fully_built of document

  let rec combine_app_chunks acc = function
    | [] -> acc
    | Function { fst_chunk; break; snd_chunk } :: rest ->
      let d1 = break_before fst_chunk in
      let d2 = if break then break_before snd_chunk else snd_chunk in
      let fn =
        break_after ~spaces:0 (nest 2 @@ (ifflat d1 (group d1)) ^^ d2)
        +++ rparen
      in
      combine_app_chunks (acc ^^ group fn) rest
    | Fully_built doc :: rest ->
      combine_app_chunks (acc ^^ nest 2 @@ group (break_before doc)) rest

  let smart_arg ps ~prefix:p lbl = function
    | { pexp_desc = Pexp_fun (params, body); pexp_attributes = []; _ } as exp ->
      let ps = [ Printing_stack.Expression exp.pexp_desc ] in
      let fun_, args, arrow, body =
        Expression.fun_chunks ~loc:exp.pexp_loc
          ~ext_attrs:exp.pexp_ext_attributes ps params body
      in
      let first_chunk =
        group ((prefix ~indent:2 ~spaces:1 fun_ args) ^/^ arrow)
      in
      let fst_chunk =
        p ^^ group @@ lparen ++ break_before ~spaces:0 first_chunk
      in
      let snd_chunk = group body in
      Function { fst_chunk; snd_chunk; break = true }
    | { pexp_desc = Pexp_function (c :: cs); pexp_attributes = []; _ } as exp ->
      let ps = [ Printing_stack.Expression exp.pexp_desc ] in
      let function_, cases =
        Expression.function_chunks ~loc:exp.pexp_loc
          ~ext_attrs:exp.pexp_ext_attributes ps c cs
      in
      let fst_chunk =
        p ^^ group @@ lparen ++ break_before ~spaces:0 function_
      in
      let snd_chunk = group cases in
      Function { fst_chunk; snd_chunk; break = false }
    | arg ->
      Fully_built (argument ps (lbl, arg))

  let smart_arg ps (lbl, exp) =
    match lbl with
    | Nolabel ->
      let prefix =
        empty ~loc:{ exp.pexp_loc with loc_end = exp.pexp_loc.loc_start }
      in
      smart_arg ps ~prefix lbl exp
    | Labelled l ->
      let prefix = tilde ++ string ~loc:l.loc (l.txt ^ ":") in
      smart_arg ps ~prefix lbl exp
    | Optional l ->
      let prefix = qmark ++ string ~loc:l.loc (l.txt ^ ":") in
      smart_arg ps ~prefix lbl exp

  let pp_simple ps applied arg args =
    let ps, enclose = Printing_stack.parenthesize ps in
    let fit_or_vertical () =
      let args = separate_map (break 1) ~f:(argument ps) arg args in
      prefix ~indent:2 ~spaces:1 applied args
    in
    let doc =
      match !Options.Applications.layout with
      | Fit_or_vertical -> fit_or_vertical ()
      | Wrap ->
        let args = List.map (argument ps) (arg :: args) in
        nest 2 @@ left_assoc_map ~f:Fun.id applied args
      | Smart ->
        let nb_labels, len_labels =
          List.fold_left (fun (nb, len) (lbl, _) ->
              match lbl with
              | Nolabel -> nb, len
              | Labelled lbl
              | Optional lbl -> nb + 1, len + String.length lbl.txt
            ) (0, 0) args
        in
        (* It would be nice if I could have "current indent + len_labels" ...
           maybe. *)
        (* HACKISH *)
        if nb_labels > 4 && len_labels > 8 then
          fit_or_vertical ()
        else
          let args = List.map (smart_arg ps) (arg :: args) in
          combine_app_chunks applied args
    in
    enclose doc

  let simple_apply ps exp arg args =
    let exp = Expression.pp ps exp in
    pp_simple ps exp arg args

  let prefix_op ps (exp, op) arg args =
    if fst arg <> Nolabel || args <> [] then
      simple_apply ps exp arg args
    else
      let ps = Printing_stack.Prefix_op :: List.tl ps in
      let ps, enclose =
        match ps with
        | [ _ ; Function_parameter ] -> ps, break_before ~spaces:1
        | _ -> Printing_stack.parenthesize ps
      in
      let op = str op in
      let arg = argument ps arg in
      let doc = nest 2 (op ^^ arg) in
      enclose doc

  let infix_op ps (exp, op) arg args =
    match arg, args with
    | (Nolabel, fst), [ (Nolabel, snd) ] ->
      let ps = Printing_stack.top_is_op ~on_left:true op.txt ps in
      let ps, enclose = Printing_stack.parenthesize ps in
      let fst = Expression.pp ps fst in
      let ps = Printing_stack.top_is_op ~on_left:false op.txt ps in
      let snd = Expression.pp ps snd in
      let doc = infix ~indent:2 ~spaces:1 (str op) fst snd in
      enclose doc
    | _ ->
      simple_apply ps exp arg args

  let classify_fun exp =
    match exp.pexp_desc with
    | Pexp_ident Lident s when s.txt <> "" -> Ident_class.classify s
    | _ -> Normal

  let pp ps exp = function
    | [] ->
      (* An application node without arguments? That can't happen. *)
      assert false
    | arg :: args ->
      match classify_fun exp with
      | Normal -> simple_apply ps exp arg args
      | Prefix_op op -> prefix_op ps (exp, op) arg args
      | Infix_op op -> infix_op ps (exp, op) arg args

end

and Expression : sig
  val pp : Printing_stack.t -> expression -> document

  val function_chunks
    : loc:Location.t
    -> ext_attrs:string loc option * attributes
    -> Printing_stack.t
    -> case
    -> case list
    -> document * document

  val fun_chunks
    : loc:Location.t
    -> ext_attrs:string loc option * attributes
    -> Printing_stack.t
    -> fun_param list
    -> expression
    -> document * document * document * document
end = struct
  let rec pp ps { pexp_desc;pexp_attributes;pexp_ext_attributes;pexp_loc;_ } =
    let ps, enclose =
      if Attribute.has_non_doc pexp_attributes then
        Printing_stack.parenthesize (Attribute :: ps)
      else
        ps, Fun.id
    in
    let ps = Printing_stack.Expression pexp_desc :: ps in
    let desc =
      group (pp_desc ~ext_attrs:pexp_ext_attributes ~loc:pexp_loc ps pexp_desc)
    in
    let doc = Attribute.attach_to_item desc pexp_attributes in
    enclose doc

  and pp_desc ~loc ~ext_attrs ps = function
    | Pexp_ident id -> pp_ident id
    | Pexp_constant c -> Constant.pp ~loc c
    | Pexp_let (rf, vbs, body) -> pp_let ~loc ~ext_attrs ps rf vbs body
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
    | Pexp_ifthenelse (branches, else_) ->
      pp_if_then_else ~loc ps branches else_
    | Pexp_sequence (e1, e2) -> pp_sequence ps e1 e2
    | Pexp_while (cond, body) -> pp_while ~loc ~ext_attrs ps cond body
    | Pexp_for (it, start, stop, dir, body) ->
      pp_for ~loc ~ext_attrs ps it start stop dir body
    | Pexp_constraint (e, ct) -> pp_constraint e ct
    | Pexp_coerce (e, ct_start, ct) -> pp_coerce e ct_start ct
    | Pexp_send (e, meth) -> pp_send ps e meth
    | Pexp_new lid -> pp_new ~loc ps lid
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

  and pp_ident = Longident.pp

  and pp_let ~ext_attrs:(extension, attrs) ~loc ps rf vbs body =
    assert (attrs = []);
    let vbs =
      let previous_vb = ref None in
      List.concat_map (fun vb ->
        let text, vb =
          let text, attrs =
            Attribute.extract_text vb.pvb_attributes
              ~item_start_pos:vb.pvb_loc.loc_start
          in
          text, { vb with pvb_attributes = attrs }
        in
        let binding = Value_binding.pp Attached_to_item vb in
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
              token_before ~start:loc.Location.loc_start lhs LET,
              extension,
              rec_token ~recursive_by_default:false rf
            | Some prev_vb ->
              token_between prev_vb lhs AND, None, None
          in
          let kw = Keyword.decorate token ~extension attrs ~later:lhs in
          match modifier with
          | None -> kw
          | Some tok ->
            let modif = token_between kw lhs tok in
            kw ^/^ modif
        in
        let binding = Binding.pp ~keyword binding in
        previous_vb := Some binding;
        Attribute.prepend_text text binding
      ) vbs
    in
    let vbs = separate hardline (List.hd vbs) (List.tl vbs) in
    let ps, enclose = Printing_stack.parenthesize ps in
    let body = pp (List.tl ps (*?*)) body in
    let in_ = token_between vbs body IN in
    enclose (group (vbs ^/^ in_) ^^ hardline ++ body)

  and case ps { pc_lhs; pc_guard; pc_rhs } =
    let lhs = Pattern.pp ~indent:2 [] pc_lhs in
    let rhs = pp ps pc_rhs in
    let lhs =
      match pc_guard with
      | None ->
        let arrow = token_between lhs rhs MINUSGREATER in
        prefix ~indent:2 ~spaces:1 lhs arrow
      | Some guard ->
        let guarded =
          let guard = pp ps guard in
          let when_ = token_between lhs guard WHEN in
          group (prefix ~indent:2 ~spaces:1 when_ guard)
        in
        let with_arrow =
          let arrow = token_between guarded rhs MINUSGREATER in
          group (guarded ^/^ arrow)
        in
        prefix ~spaces:1 ~indent:2 lhs with_arrow
    in
    match !Options.Cases.body_on_separate_line with
    | Always -> lhs ^^ nest !Options.Cases.body_indent (hardline ++ rhs)
    | When_needed -> prefix ~indent:!Options.Cases.body_indent ~spaces:1 lhs rhs

  and cases ps c cs =
    let cases =
      (* FIXME *)
      separate_map PPrint.(break 1 ^^ bar ^^ space) ~f:(case ps) c cs
    in
    let prefix =
      let open PPrint in
      ifflat empty (hardline ^^ bar) ^^ space
    in
    prefix ++ cases

  and function_chunks ~loc ~ext_attrs:(extension, attrs) ps c cs =
    let cases = cases ps c cs in
    let keyword =
      let kw = token_before ~start:loc.Location.loc_start cases FUNCTION in
      Keyword.decorate kw ~extension attrs ~later:cases
    in
    keyword, cases

  and pp_function ~loc ~ext_attrs ps = function
    | [] -> assert false (* always at least one case *)
    | c :: cs ->
      let ps, enclose = Printing_stack.parenthesize ps in
      let keyword, cases = function_chunks ~loc ~ext_attrs ps c cs in
      enclose (keyword ^^ cases)

  and fun_syntactic_elts ~loc ~ext_attrs:(extension, attrs) ~args body =
    let kw = token_before ~start:loc.Location.loc_start args FUN in
    let kw = Keyword.decorate kw ~extension attrs ~later:args in
    let arrow = token_between args body MINUSGREATER in
    kw, arrow

  and fun_chunks ~loc ~ext_attrs ps params exp =
    match params with
    | [] -> assert false
    | param :: params ->
      let args = left_assoc_map ~f:Fun_param.pp param params in
      let body = pp ps exp in
      let kw, arrow = fun_syntactic_elts ~loc ~ext_attrs ~args body in
      kw, args, arrow, body

  and pp_fun ~loc ~ext_attrs ps params exp =
    let ps, enclose = Printing_stack.parenthesize ps in
    let fun_, args, arrow, body = fun_chunks ~loc ~ext_attrs ps params exp in
    let doc =
      prefix ~indent:2 ~spaces:1
        (group ((prefix ~indent:2 ~spaces:1 fun_ args) ^/^ arrow))
        body
    in
    enclose doc

  and pp_match ~loc ~ext_attrs:(extension, attrs) ps arg = function
    | [] -> assert false (* always at least one case *)
    | c :: cs ->
      let arg = pp [] arg in
      let ps, enclose =
        Printing_stack.parenthesize ps
          ~situations:!Options.Match.parenthesing_situations
          ~style:!Options.Match.parens_style
      in
      let cases = cases ps c cs in
      let match_ =
        let token = token_before ~start:loc.Location.loc_start arg MATCH in
        Keyword.decorate token ~extension attrs ~later:arg
      in
      let with_ = token_between arg cases WITH in
      let doc =
        group (
          match_ ^^
          nest 2 (break_before arg) ^/^
          with_
        ) ^^ cases
      in
      enclose doc

  and pp_try ~loc ~ext_attrs:(extension, attrs) ps arg = function
    | [] -> assert false
    | c :: cs ->
      let arg = pp [] arg in
      let ps, enclose = Printing_stack.parenthesize ps in
      let cases = cases ps c cs in
      let try_ =
        let token = token_before ~start:loc.Location.loc_start arg TRY in
        Keyword.decorate token ~extension attrs ~later:arg
      in
      let with_ = token_between arg cases WITH in
      let doc =
        group (
          try_ ^^
          nest 2 (break_before arg)
        ) ^/^
        with_ ^^
        cases
      in
      enclose doc

  and pp_tuple ps = function
    | [] -> assert false
    | exp :: exps ->
      let ps, enclose = Printing_stack.parenthesize ps in
      let doc =
        group (separate_map PPrint.(comma ^^ break 1) ~f:(pp ps) exp exps)
      in
      enclose doc

  and pp_construct ps lid arg_opt =
    let name = Longident.pp lid in
    match arg_opt with
    | None -> name
    | Some arg ->
      let ps, enclose = Printing_stack.parenthesize ps in
      let arg  = pp ps arg in
      let doc  = prefix ~indent:2 ~spaces:1 name arg in
      enclose doc

  and pp_cons ps hd tl =
    let ps = Printing_stack.top_is_op ~on_left:false "::" ps in
    let ps, enclose = Printing_stack.parenthesize ps in
    let hd =
      let ps = Printing_stack.top_is_op ~on_left:true "::" ps in
      Expression.pp ps hd
    in
    let tl = Expression.pp ps tl in
    let cons = token_between hd tl COLONCOLON in
    let doc = infix ~indent:2 ~spaces:1 cons hd tl in
    enclose doc

  and pp_list_literal ~loc ps elts =
    let ps = [ List.hd ps ] in
    let elts = List.map (pp ps) elts in
    List_like.pp ~loc
      ~formatting:Wrap (* TODO: add an option *)
      ~left:lbracket ~right:rbracket
      elts

  and pp_variant ps tag arg_opt =
    let tag = Polymorphic_variant_tag.pp tag in
    match arg_opt with
    | None -> tag
    | Some arg ->
      let ps, enclose = Printing_stack.parenthesize ps in
      let arg  = pp ps arg in
      let doc  = prefix ~indent:2 ~spaces:1 tag arg in
      enclose doc

  and record_field (lid, (oct1, oct2), exp) =
    let params =
      let pos = (Longident.endpos lid).loc_end in
      { txt = []; loc = { loc_start = pos; loc_end = pos }}
    in
    let binding : Binding.t =
      { lhs = Longident.pp lid;
        params;
        constr = Option.map (Core_type.pp []) oct1;
        coerce = Option.map (Core_type.pp []) oct2;
        rhs = Option.map (pp [ Printing_stack.Record_field ]) exp;
      }
    in
    Binding.pp binding

  and pp_record ~loc ps fields updated_record =
    let fields = List.map record_field fields in
    match updated_record with
    | None ->
      List_like.pp ~loc
        ~formatting:!Options.Record.expression
        ~left:lbrace
        ~right:rbrace
        fields
    | Some e ->
      let update = pp ps e in
      let fields =
        List_like.pp_fields ~formatting:!Options.Record.expression
          (List.hd fields) (List.tl fields)
      in
      let with_ = token_between update fields WITH in
      enclose ~before:lbrace ~after:PPrint.(break 1 ^^ rbrace)
        (group (group (break_before update) ^/^ with_) ^/^ fields)

  and pp_field ps re fld =
    let ps, enclose = Printing_stack.parenthesize ps in
    let record = pp ps re in
    let field = Longident.pp fld in
    let dot = token_between record field DOT in
    let doc = flow (break 0) record [ dot; field ] in
    enclose doc

  and pp_setfield ps re fld val_ =
    let field =
      let ps = Printing_stack.Expression (Pexp_field (re, fld)) :: List.tl ps in
      pp_field ps re fld
    in
    let ps, enclose = Printing_stack.parenthesize ps in
    let value = pp (List.tl ps) val_ in
    let larrow = token_between field value LESSMINUS in
    let doc =
      prefix ~indent:2 ~spaces:1
        (group (field ^/^ larrow))
        value
    in
    enclose doc

  and pp_array ~loc ps elts =
    let elts = List.map (pp ps) elts in
    (* TODO: add an option *)
    List_like.pp ~loc ~formatting:Wrap
      ~left:PPrint.(lbracket ^^ bar)
      ~right:PPrint.(bar ^^ rbracket) elts

  and pp_gen_get ?prefix ?dot enclosing ps arr idx =
    let ps, enclose = Printing_stack.parenthesize ps in
    let arr = pp ps arr in
    let dot =
      match prefix, dot with
      | None, None -> token_between arr idx DOT
      | Some path, Some dotop ->
        let fstdot = token_between arr path DOT in
        group (fstdot ^^ path ^^ break_before ~spaces:0 dotop)
      | None, Some dotop -> dotop
      | Some _, None -> assert false
    in
    let doc = flow (break 0) arr [ dot; enclosing idx ] in
    enclose doc

  and pp_gen_set ?prefix:p ?dot enclosing access_ps ps arr idx val_ =
    let access = pp_gen_get ?prefix:p ?dot enclosing access_ps arr idx in
    let ps, enclose = Printing_stack.parenthesize ps in
    let value = pp (List.tl ps) val_ in
    let larrow = token_between access value LESSMINUS in
    let doc =
      prefix ~indent:2 ~spaces:1
        (group (access ^/^ larrow))
        value
    in
    enclose doc

  and pp_array_get ps arr idx = pp_gen_get parens ps arr (pp [] idx)
  and pp_array_set ps arr idx val_ =
    let access_ps =
      Printing_stack.Expression (Pexp_array_get (arr, idx)) :: List.tl ps
    in
    pp_gen_set parens access_ps ps arr (pp [] idx) val_

  and pp_string_get ps arr idx = pp_gen_get brackets ps arr (pp [] idx)
  and pp_string_set ps arr idx val_ =
    let access_ps =
      Printing_stack.Expression (Pexp_string_get (arr, idx)) :: List.tl ps
    in
    pp_gen_set brackets access_ps ps arr (pp [] idx) val_

  and pp_bigarray_get ps arr idx =
    let idx = pp_tuple ps idx in
    pp_gen_get braces ps arr idx

  and pp_bigarray_set ps arr idx val_ =
    let access_ps =
      Printing_stack.Expression (Pexp_bigarray_get (arr, idx)) :: List.tl ps
    in
    let idx = pp_tuple ps idx in
    pp_gen_set braces access_ps ps arr idx val_

  and pp_dotop_get ps accessed op left right indices =
    let enclose doc = str left ^^ doc ^^ str right in
    let indices =
      match indices with
      | [] -> assert false (* I think *)
      | idx :: ids -> separate_map semi ~f:(pp []) idx ids
    in
    let prefix, op =
      match op with
      | Lident s -> None, str s
      | Ldot (lid, s) -> Some (Longident.pp lid), str s
      | Lapply _ -> assert false
    in
    pp_gen_get ?prefix ~dot:(!^"." ++ op) enclose ps accessed indices

  and pp_dotop_set ps accessed op left right indices val_ =
    let enclose doc = group (Longident.pp op ^^ str left) ^^ doc ^^ str right in
    let access_ps =
      [ Printing_stack.Expression
          (Pexp_dotop_get { accessed; op; left; right; indices }) ]
    in
    let indices =
      match indices with
      | [] -> assert false (* I think *)
      | idx :: ids -> separate_map semi ~f:(pp []) idx ids
    in
    let prefix, op =
      match op with
      | Lident s -> None, str s
      | Ldot (lid, s) -> Some (Longident.pp lid), str s
      | Lapply _ -> assert false
    in
    pp_gen_set ?prefix ~dot:(!^"." ++ op) enclose access_ps ps accessed indices
      val_

  and pp_if_then_else ~loc ps if_branches else_opt =
    let ps, enclose = Printing_stack.parenthesize ps in
    let if_branches =
      let ps = Printing_stack.Then_branch :: List.tl ps in
      List.mapi (fun i ib ->
        let cond = pp [] ib.if_cond in
        let then_branch = pp ps ib.if_body in
        let then_kw = token_between cond then_branch THEN in
        let keyword =
          let if_kw =
            let if_ = token_before ~start:ib.if_loc.loc_start cond IF in
            if i = 0 then if_ else !^"else " ++ if_
          in
          let with_ext =
            match ib.if_ext with
            | None -> if_kw
            | Some { txt = ext_name ; loc } ->
              let tag = string ~loc ("%" ^ ext_name) in
              if_kw ^^ brackets tag
          in
          let with_attrs = Attribute.attach_to_item with_ext ib.if_attrs in
          with_attrs
        in
        group (
          keyword ^^
          nest 2 (break_before cond) ^/^
          then_kw
        ) ^^
        nest 2 (break_before then_branch)
      ) if_branches
    in
    let if_ =
      separate (PPrint.break 1) (List.hd if_branches) (List.tl if_branches)
    in
    let else_ =
      let loc = { loc with Location.loc_start = if_.loc.loc_end } in
      optional ~loc (fun e ->
        let else_branch = pp ps e in
        let else_ = token_between if_ else_branch ELSE in
        break_before else_ ^^
        nest 2 (break_before else_branch)
      ) else_opt
    in
    let doc = group (if_ ^^ else_) in
    enclose doc

  and pp_sequence ps e1 e2 =
    let ps, enclose = Printing_stack.parenthesize ps in
    let e1 = pp ps e1 in
    let e2 = pp (List.tl ps) e2 in
    let semi = token_between e1 e2 SEMI in
    let doc = e1 ^^ semi ^/^ e2 in
    enclose doc

  and pp_while ~(loc:Location.t) ~ext_attrs:(extension, attrs) ps cond body =
    let cond = pp [] cond in
    let body = pp [] body in
    let do_ = token_between cond body DO in
    let while_ = token_before ~start:loc.loc_start cond WHILE in
    let while_ = Keyword.decorate while_ ~extension attrs ~later:cond in
    let done_ = token_after body ~stop:loc.loc_end DONE in
    let doc =
      group (
        group (
          while_ ^^
          nest 2 (break_before cond) ^/^
          do_
        ) ^^
        nest 2 (break_before body) ^/^
        done_
      )
    in
    let _, enclose = Printing_stack.parenthesize ps in
    enclose doc

  and pp_for ~(loc:Location.t) ~ext_attrs:(extension, attrs) ps it start stop
      dir body =
    let it = Pattern.pp [ Printing_stack.Value_binding ] it in
    let start = pp [] start in
    let equals = token_between it start EQUAL in
    let stop = pp [] stop in
    let dir =
      token_between start stop
        (match dir with
         | Upto -> TO
         | Downto -> DOWNTO)
    in
    let body = pp [] body in
    let do_ = token_between stop body DO in
    let loc_start = { loc with loc_end = it.loc.loc_start } in
    let loc_end = { loc with loc_start = body.loc.loc_end } in
    let for_ = string ~loc:loc_start "for" in
    let for_ = Keyword.decorate for_ ~extension attrs ~later:it in
    let doc =
      group (
        group (
          for_ ^^
          nest 2 (
            break_before (group (it ^/^ equals ^/^ start)) ^/^
            dir ^/^
            stop
          ) ^/^
          do_
        ) ^^
        nest 2 (break_before body) ^/^
        string ~loc:loc_end "done"
      )
    in
    let _, enclose = Printing_stack.parenthesize ps in
    enclose doc

  and pp_constraint exp ct =
    let exp = pp [] exp in
    let ct = Core_type.pp [] ct in
    let colon = token_between exp ct COLON in
    group (parens (exp ^/^ colon ^/^ ct))

  and pp_coerce exp ct_start ct =
    let exp = pp [] exp in
    let ct = Core_type.pp [] ct in
    let ct_start =
      let loc = { exp.loc with loc_start = exp.loc.loc_end } in
      optional ~loc (fun ct ->
        let ct = Core_type.pp [] ct in
        let colon = token_between exp ct COLON in
        break_before colon ^/^ ct
      ) ct_start
    in
    let coerce = token_between ct_start ct COLONGREATER in
    group (parens (group (exp ^^ ct_start) ^/^ coerce ^/^  ct))

  and pp_send ps exp met =
    let ps = Printing_stack.top_is_op ~on_left:true "#" ps in
    let ps, enclose = Printing_stack.parenthesize ps in
    let exp = pp ps exp in
    let met = str met in
    let sharp = token_between exp met HASH in
    let doc = flow (break 0) exp [ sharp; met ] in
    enclose doc

  and pp_new ps ~loc lid =
    let lid = Longident.pp lid in
    let new_ = token_before ~start:loc.loc_start lid NEW in
    let _, enclose = Printing_stack.parenthesize ps in
    enclose (group (new_ ^/^ lid))


  and pp_setinstvar ps lbl exp =
    let ps, enclose = Printing_stack.parenthesize ps in
    let lbl = str lbl in
    let exp = pp (List.tl ps) exp in
    let larrow = token_between lbl exp LESSMINUS in
    let doc = lbl ^/^ larrow ^/^ exp in
    enclose doc

  and obj_field_override (lbl, exp) =
    let fld = str lbl in
    match exp.pexp_desc with
    | Pexp_ident Lident s when s.txt = lbl.txt -> fld
    | _ ->
      let exp = pp [ Printing_stack.Record_field ] exp in
      let equals = token_between fld exp EQUAL in
      fld ^/^ equals ^/^ exp

  and pp_override ~loc fields =
    List_like.pp ~loc
      ~formatting:!Options.Record.expression
      ~left:PPrint.(lbrace ^^ langle)
      ~right:PPrint.(rangle ^^ rbrace)
      (List.map obj_field_override fields)

  and pp_letmodule ~loc ~ext_attrs:(extension, attrs) ps name
      (params, typ, mexp) expr =
    let binding = Module_binding.pp_raw name params typ mexp [] in
    let bind =
      let keyword =
        let let_ = token_before ~start:loc.loc_start binding.name LET in
        let mod_ = token_between let_ binding.name MODULE in
        Keyword.decorate (let_ ^/^ mod_) ~extension attrs ~later:binding.name
      in
      Binding.Module.pp ~keyword ~context:Struct binding
    in
    let ps, enclose = Printing_stack.parenthesize ps in
    let expr = pp (List.tl ps) expr in
    let in_ = token_between bind expr IN in
    let doc = bind ^/^ in_ ^/^ expr in
    enclose doc

  and pp_letexception ~(loc:Location.t) ~ext_attrs:(extension, attrs) ps exn
      exp =
    let exn = Constructor_decl.pp_extension exn in
    let ps, enclose = Printing_stack.parenthesize ps in
    let exp = pp (List.tl ps) exp in
    let keyword =
      let let_ = token_before ~start:loc.loc_start exn LET in
      let exc = token_between let_ exn EXCEPTION in
      Keyword.decorate (let_ ^/^ exc) ~extension attrs ~later:exn
    in
    let in_ = token_between exn exp IN in
    let doc =
      group (prefix ~indent:2 ~spaces:1 keyword
               (group (exn ^/^ in_)))
      ^/^ exp
    in
    enclose doc

  and pp_assert ~(loc:Location.t) ~ext_attrs:(extension, attrs) ps exp =
    let ps, enclose = Printing_stack.parenthesize ps in
    let exp = pp ps exp in
    let assert_ =
      let loc = { loc with loc_end = exp.loc.loc_start } in
      Keyword.decorate (string ~loc "assert")
        ~extension attrs ~later:exp
    in
    let doc = prefix ~indent:2 ~spaces:1 assert_ exp in
    enclose doc

  and pp_lazy ~(loc:Location.t) ~ext_attrs:(extension, attrs) ps exp =
    let ps, enclose = Printing_stack.parenthesize ps in
    let exp = pp ps exp in
    let lazy_ =
      let loc = { loc with loc_end = exp.loc.loc_start } in
      Keyword.decorate (string ~loc "lazy")
        ~extension attrs ~later:exp
    in
    let doc = prefix ~indent:2 ~spaces:1 lazy_ exp in
    enclose doc

  and pp_object ~loc ~ext_attrs ps cs =
    let doc = Class_structure.pp ~loc ~ext_attrs cs in
    let _, enclose = Printing_stack.parenthesize ps in
    enclose doc

  and pp_pack ~loc ~ext_attrs:(extension, attrs) me pkg =
    let me = Module_expr.pp me in
    let with_constraint =
      match pkg with
      | None -> me
      | Some pkg ->
        let constr = Package_type.pp pkg in
        let colon = token_between me constr COLON in
        me ^/^ colon ^/^ constr
    in
    let mod_ =
      token_before ~start:loc.Location.loc_start with_constraint MODULE
    in
    let mod_ = Keyword.decorate mod_ ~extension attrs ~later:with_constraint in
    (* FIXME: comments between "(" and "module" are going to be moved ... *)
    group (lparen ++ mod_) ^/^ with_constraint +++ !^")"

  and pp_open lid exp =
    let lid = Longident.pp lid in
    let exp = pp [] exp in
    let dot = token_between lid exp DOT in
    let exp =
      enclose (nest 2 @@ break_before ~spaces:0 exp)
        ~before:PPrint.lparen
        ~after:PPrint.(break 0 ^^ rparen)
    in
    lid ^^ dot ^^ exp

  and pp_letopen ~(loc:Location.t) ~ext_attrs ps od exp =
    let od = Open_declaration.pp ~ext_attrs Attached_to_item od in
    let ps, enclose = Printing_stack.parenthesize ps in
    let exp = pp (List.tl ps) exp in
    let in_ = token_between od exp IN in
    let let_ =
      let loc = { loc with loc_end = od.loc.loc_start } in
      string ~loc "let"
    in
    let doc = group (let_ ^/^ od ^/^ in_) ^/^ exp in
    enclose doc

  and pp_binding_op (bop : binding_op) =
    let binding = Value_binding.pp_bop Attached_to_item bop in
    let keyword = Longident.pp_ident bop.pbop_op in
    Binding.pp ~keyword binding

  and pp_letop ps { let_; ands; body } =
    let let_ = pp_binding_op let_ in
    let ands = List.map pp_binding_op ands in
    let bindings = separate hardline let_ ands in
    let ps, enclose = Printing_stack.parenthesize ps in
    let body = pp (List.tl ps) body in
    let in_ = token_between bindings body IN in
    enclose (group (bindings ^/^ in_) ^^ hardline ++ body)
end

and Fun_param : sig
  val pp : fun_param -> document
end = struct
  let punned_label_with_annot prefix_token lbl ct =
    let lbl = str lbl in
    let ct = Core_type.pp [] ct in
    let colon = token_between lbl ct COLON in
    prefix_token ++ parens (lbl ^^ colon ^^ break_before ~spaces:0 ct)

  let build_simple_label ~optional lbl (pat_opt, cty_opt) =
    let prefix_token = if optional then qmark else tilde in
    match pat_opt, cty_opt with
    | None, None ->
      prefix_token ++ str lbl
    | None, Some ct ->
      punned_label_with_annot prefix_token lbl ct
    | Some pat, None ->
      let fresh_stack =
        Printing_stack.[
          if optional then Function_parameter else Value_binding
        ]
      in
      let pat = Pattern.pp fresh_stack pat in
      let loc = lbl.loc in
      begin match token_before ~start:loc.loc_end pat COLON with
      | colon -> prefix_token ++ str lbl ^^ colon ^^ pat
      | exception (Not_found | Assert_failure _ (* gloups. *)) ->
        let label = string ~loc (lbl.txt ^ ":") in
        prefix_token ++ label ^^ pat
      end
    | Some pat, Some ct ->
      let fresh_stack =
        Printing_stack.[
          if optional then Function_parameter else Value_binding
        ]
      in
      let pat = Pattern.pp fresh_stack pat in
      let ct = Core_type.pp [] ct in
      let rhs =
        let colon = token_between pat ct COLON in
        parens (pat ^^ colon ^^ ct)
      in
      match token_before ~start:lbl.loc.loc_end pat COLON with
      | colon -> prefix_token ++ str lbl ^^ colon ^^ rhs
      | exception (Not_found | Assert_failure _ (* gloups. *)) ->
        let label = string ~loc:lbl.loc (lbl.txt ^ ":") in
        prefix_token ++ label ^^ rhs

  let build_optional_with_default lbl def (pat_opt, ct_opt) =
    let lbl = str lbl in
    let def = Expression.pp [ Printing_stack.Function_parameter ] def in
    qmark ++ match pat_opt, ct_opt with
    | None, None ->
      let eq = token_between lbl def EQUAL in
      parens (lbl ^^ eq ^^ def)
    | None, Some ct ->
      let ct = Core_type.pp [] ct in
      let colon = token_between lbl def COLON in
      let eq = token_between ct def EQUAL in
      parens (lbl ^^ colon ^^ ct ^^ eq ^^ def)
    | Some pat, None ->
      let fresh_stack = [ Printing_stack.Value_binding ] in
      let pat = Pattern.pp fresh_stack pat in
      let eq = token_between pat def EQUAL in
      lbl ^^ colon ++ break_before ~spaces:0 (parens (group (pat ^^ eq ^^ def)))
    | Some pat, Some ct ->
      let fresh_stack = [ Printing_stack.Value_binding ] in
      let pat = Pattern.pp fresh_stack pat in
      let ct = Core_type.pp [] ct in
      let eq = token_between ct def EQUAL in
      let col = token_between pat ct COLON in
      lbl ^^ colon ++ break_before ~spaces:0
               (parens (group (pat ^^ col ^^ ct ^^ eq ^^ def)))


  let term lbl default pat_and_ty =
    match lbl with
    | Nolabel ->
      begin match pat_and_ty with
      | Some pat, None -> Pattern.pp [ Printing_stack.Value_binding ] pat
      | _ -> assert false
      end
    | Labelled lbl -> build_simple_label ~optional:false lbl pat_and_ty
    | Optional lbl ->
      match default with
      | None -> build_simple_label ~optional:true lbl pat_and_ty
      | Some def -> build_optional_with_default lbl def pat_and_ty

  let newtype typ =
    parens (!^"type " ++ str typ)

  let pp = function
    | Term (lbl, default, pat) -> group (term lbl default pat)
    | Type typ -> group (newtype typ)
end

and Value_binding : sig
  val pp_bop : Attribute.kind -> binding_op -> Binding.t
  val pp : Attribute.kind -> value_binding -> Binding.t
end = struct

  let pp_raw attr_kind pvb_pat pvb_params pvb_type pvb_expr pvb_attributes =
    let pat = Pattern.pp [ Printing_stack.Value_binding ] pvb_pat in
    let params = List.map Fun_param.pp pvb_params in
    let constr, coerce = pvb_type in
    let constr = Option.map (Core_type.pp [ Value_binding ]) constr in
    let coerce = Option.map (Core_type.pp [ Value_binding ]) coerce in
    let rhs = Expression.pp [] pvb_expr in
    let rhs = Some (Attribute.attach attr_kind rhs pvb_attributes) in
    let params =
      let loc = { pat.loc with loc_start = pat.loc.loc_end } in
      { txt = params; loc }
    in
    { Binding.lhs = pat; params; constr; coerce; rhs }

  let pp_bop attr_kind { pbop_pat; pbop_params; pbop_type; pbop_exp; _ } =
    pp_raw attr_kind pbop_pat pbop_params pbop_type pbop_exp []

  let pp attr_kind
      { pvb_pat; pvb_params; pvb_type; pvb_expr; pvb_attributes; _ } =
    pp_raw attr_kind
      pvb_pat pvb_params pvb_type pvb_expr pvb_attributes
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
        let colon = token_between name mty COLON in
        parens (group (name ^/^ colon) ^/^ mty)
      in
      match name.txt with
      | None ->
        let name = token_before ~start:loc.loc_start mty UNDERSCORE in
        pp name
      | Some s ->
        let name = string ~loc:name.loc s in
        pp name
end

and Module_expr : sig
  val pp : module_expr -> document
end = struct
  let rec pp ?(lhs_of_apply=false) { pmod_desc; pmod_attributes; pmod_loc } =
    let doc = pp_desc ~loc:pmod_loc pmod_desc in
    let doc = Attribute.attach_to_item doc pmod_attributes in
    match lhs_of_apply, pmod_desc with
    | true, Pmod_functor _ -> parens doc
    | _ -> doc

  and pp_desc ~loc = function
    | Pmod_ident lid -> Longident.pp lid
    | Pmod_structure str -> pp_structure ~loc str
    | Pmod_functor (params, me) -> pp_functor ~loc params me
    | Pmod_apply (me1, me2) -> pp_apply me1 me2
    | Pmod_constraint (me, mty) -> pp_constraint me mty
    | Pmod_unpack e -> pp_unpack ~loc e
    | Pmod_extension ext -> Extension.pp Item ext

  and pp_structure ~loc = function
    | [] -> string ~loc "struct end"
    | si :: st ->
      let str = Structure.pp_nonempty si st in
      group (
        enclose ~before:!^"struct" ~after:PPrint.(break 1 ^^ !^"end")
          (nest 2 (break_before str))
      )

  and pp_functor ~(loc:Location.t) params me =
    let params =
      separate_map (PPrint.break 1)
        ~f:Functor_param.pp (List.hd params) (List.tl params)
    in
    let me = pp me in
    let functor_ =
      let loc = { loc with loc_end = params.loc.loc_start } in
      string ~loc "functor"
    in
    let arrow = token_between params me MINUSGREATER in
    functor_ ^/^ params ^/^ arrow ^/^ me

  and pp_apply me1 me2 =
    let me1 = pp ~lhs_of_apply:true me1 in
    let me2 = pp me2 in
    me1 ^^ break_before ~spaces:0 (parens me2)

  and pp_constraint me mty =
    let me = pp me in
    let mty = Module_type.pp mty in
    let colon = token_between me mty COLON in
    parens (me ^/^ colon ^/^ mty)

  and pp_unpack ~(loc:Location.t) exp =
    let exp = Expression.pp [ Unpack ] exp in
    let val_=
      let loc = { loc with loc_end = exp.loc.loc_start } in
      string ~loc "val"
    in
    parens (val_ ^/^ exp)

  let pp = pp ~lhs_of_apply:false
end

and Module_type : sig
  val pp : module_type -> document
end = struct
  let rec pp { pmty_desc; pmty_attributes; pmty_loc; _ } =
    let ps, enclose =
      if Attribute.has_non_doc pmty_attributes then
        Printing_stack.parenthesize [ Attribute ]
      else
        [], Fun.id
    in
    let ps = Printing_stack.Module_type pmty_desc :: ps in
    let doc = group (pp_desc ~loc:pmty_loc ps pmty_desc) in
    let doc = Attribute.attach_to_item doc pmty_attributes in
    enclose doc

  and pp_desc ~loc ps = function
    | Pmty_alias lid (* [module type _ = A] *)
    | Pmty_ident lid (* [module _ : A] *)
      -> Longident.pp lid
    | Pmty_signature sg -> pp_signature ~loc sg
    | Pmty_functor (params, mty) -> pp_functor ~loc params mty
    | Pmty_with (mty, cstrs) -> pp_with mty cstrs
    | Pmty_typeof me -> pp_typeof ps me
    | Pmty_extension ext -> Extension.pp Item ext

  and pp_signature ~loc = function
    | [] -> string ~loc "sig end"
    | si :: sg ->
      let sg = Signature.pp_nonempty si sg in
      let sig_ = token_before ~start:loc.loc_start sg SIG in
      let end_ = token_after ~stop:loc.loc_end sg END in
      (prefix ~indent:2 ~spaces:1 sig_ sg) ^/^ end_

  and pp_short_functor param_mty res_mty =
    let param_mty = Module_type.pp param_mty in
    let res_mty = Module_type.pp res_mty in
    let arrow = token_between param_mty res_mty MINUSGREATER in
    param_mty ^/^ arrow ^/^ res_mty

  and pp_regular_functor ~(loc:Location.t) params mty =
    let params =
      separate_map (PPrint.break 1)
        ~f:Functor_param.pp (List.hd params) (List.tl params)
    in
    let mty = pp mty in
    let functor_ =
      let loc = { loc with loc_end = params.loc.loc_start } in
      string ~loc "functor"
    in
    let arrow = token_between params mty MINUSGREATER in
    functor_ ^/^ params ^/^ arrow ^/^ mty

  and pp_functor ~loc params mty =
    match params with
    | [ { txt = Named ({ txt = None; _ }, param_mty); _ } ] ->
      pp_short_functor param_mty mty
    | _ ->
      pp_regular_functor ~loc params mty

  and attach_constraint mty is_first_cstr (kw, cstr) =
    let keyword =
      match kw with
      | With loc -> string ~loc "with"
      | And  loc -> string ~loc "and"
    in
    let cstr =
      match cstr with
      | Pwith_type (lid, td) ->
        let type_ = token_after keyword ~stop:td.ptype_loc.loc_start TYPE in
        let kw = keyword ^/^ type_ in
        Type_declaration.pp_with_constraint ~override_name:(Longident.pp lid)
          ~keyword:(Formatted kw) td
      | Pwith_typesubst (lid, td) ->
        let type_ = token_after keyword ~stop:td.ptype_loc.loc_start TYPE in
        let kw = keyword ^/^ type_ in
        Type_declaration.pp_with_constraint ~binder:COLONEQUAL
          ~override_name:(Longident.pp lid)
          ~keyword:(Formatted kw) td
      | Pwith_module (lid1, lid2) ->
        let d1 = Longident.pp lid1 in
        let d2 = Longident.pp lid2 in
        let module_ = token_between keyword d1 MODULE in
        let keyword = keyword ^/^ module_ in
        Binding.pp_simple  ~keyword d1 d2
      | Pwith_modsubst (lid1, lid2) ->
        let d1 = Longident.pp lid1 in
        let d2 = Longident.pp lid2 in
        let module_ = token_between keyword d1 MODULE in
        let keyword = keyword ^/^ module_ in
        Binding.pp_simple ~binder:COLONEQUAL ~keyword d1 d2
    in
    if is_first_cstr then
      prefix ~spaces:1 ~indent:2 mty cstr
    else
      let indent =
        match kw with
        | With _ -> 2
        | And _ -> 3
      in
      mty ^^ nest indent (break_before cstr)

  and pp_with mty cstrs =
    let mty = pp mty in
    let with_constraints, _ =
      List.fold_left (fun (mty, is_first) cstr ->
        let mty = attach_constraint mty is_first cstr in
        mty, false
      ) (mty, true) cstrs
    in
    with_constraints

  and pp_typeof ps exp =
    let _, enclose = Printing_stack.parenthesize ps in
    let me = Module_expr.pp exp in
    let pre = PPrint.flow (break 1) [ !^"module"; !^"type"; !^"of" ] in
    pre ++ break_before me
    |> enclose

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
      let colon = token_between name mty COLON in
      group (
        parens (
          prefix ~indent:2 ~spaces:1 (group (name ^/^ colon)) mty
        )
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
    | Pmod_structure (si :: st), [] ->
      Binding.Module.Items (Structure.pp_nonempty si st)
    | _ -> Binding.Module.Generic (Module_expr.pp me)

  let pp_raw name params mty me attrs =
    let name = module_name name in
    let params = List.map param params in
    let constr = pp_mty mty in
    let body = pp_me me in
    let attributes =
      match attrs with
      | [] -> empty ~loc:{ me.pmod_loc with loc_start = me.pmod_loc.loc_end }
      | attr :: attrs ->
        separate_map (break 0) ~f:(Attribute.pp Attached_to_structure_item)
          attr attrs
    in
    let params =
      let loc = { name.loc with loc_start = name.loc.loc_end } in
      { loc; txt = params }
    in
    { Binding.Module. name; params; constr; body; attributes }

  let pp { pmb_name; pmb_params; pmb_type; pmb_expr; pmb_attributes; _ } =
    let binding = pp_raw pmb_name pmb_params pmb_type pmb_expr pmb_attributes in
    let body = binding.body in
    { binding with body }
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

  let pp_raw { pmd_name; pmd_params; pmd_type; pmd_attributes; pmd_loc = _ } =
    let name = module_name pmd_name in
    let params = List.map Module_binding.param pmd_params in
    let body = pp_mty pmd_type in
    let attributes =
      match pmd_attributes with
      | [] ->
        empty ~loc:{ pmd_type.pmty_loc with loc_start = pmd_type.pmty_loc.loc_end }
      | attr :: attrs ->
        separate_map (break 0) ~f:(Attribute.pp Attached_to_structure_item)
          attr attrs
    in
    let params =
      let loc = { name.loc with loc_start = name.loc.loc_end } in
      { loc; txt = params }
    in
    { Binding.Module. name; params; constr = None; body; attributes }

  let decide_context pmd_type : Binding.Module.context =
    (* This is a hack.
       The context is used to decide whether to print "=" or ":", but that
       doesn't quite work: module aliases declarations use "=". *)
    match pmd_type.pmty_desc with
    | Pmty_alias _ -> Struct
    | _ -> Sig

  let pp pmd =
    let kw =
      let loc = { pmd.pmd_loc with loc_end = pmd.pmd_name.loc.loc_start } in
      string ~loc "module"
    in
    let text, pmd =
      let text, attrs =
        Attribute.extract_text ~item_start_pos:pmd.pmd_loc.loc_start
          pmd.pmd_attributes
      in
      text, { pmd with pmd_attributes = attrs }
    in
    let binding = pp_raw pmd in
    let context = decide_context pmd.pmd_type in
    let binding = Binding.Module.pp ~keyword:kw ~context binding in
    let docs = Attribute.prepend_text text binding in
    separate (twice hardline) (List.hd docs) (List.tl docs)
end

and Module_substitution : sig
  val pp : module_substitution -> document
end = struct
  let pp { pms_name; pms_manifest; pms_attributes; pms_loc } =
    let kw =
      let loc = { pms_loc with loc_end = pms_name.loc.loc_start } in
      string ~loc "module "
    in
    let name = str pms_name in
    let man = Longident.pp pms_manifest in
    let doc = Binding.pp_simple ~keyword:kw name ~binder:COLONEQUAL man in
    Attribute.attach_to_top_item doc pms_attributes
end

and Module_type_declaration : sig
  val pp : module_type_declaration -> document
end = struct
  let pp { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc } =
    let text, pmtd_attributes =
      Attribute.extract_text pmtd_attributes ~item_start_pos:pmtd_loc.loc_start
    in
    let kw =
      let loc = { pmtd_loc with loc_end = pmtd_name.loc.loc_start } in
      string ~loc "module type"
    in
    let name = str pmtd_name in
    let doc =
      match pmtd_type with
      | None -> kw ^/^ name
      | Some mty ->
        let typ = Module_type.pp mty in
        Binding.pp_simple ~keyword:kw name typ
    in
    let decl =
      Attribute.attach_to_top_item doc pmtd_attributes
      |> Attribute.prepend_text text
    in
    separate (twice hardline) (List.hd decl) (List.tl decl)
end

and Structure : sig
  val pp_nonempty : structure_item -> structure -> document
end = struct
  let pp_eval ~first exp attrs =
    let exp = Expression.pp [] exp in
    let doc =  Attribute.attach_to_top_item exp attrs in
    if first
    then doc
    else !^";; " ++ doc

  let pp_value ~loc rf vbs =
    let vbs =
      let previous_vb = ref None in
      List.concat_map (fun vb ->
        let text, vb =
          let text, attrs =
            Attribute.extract_text ~item_start_pos:vb.pvb_loc.loc_start
              vb.pvb_attributes
          in
          text, { vb with pvb_attributes = attrs }
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
              token_before ~start:loc.Location.loc_start lhs LET,
              rec_token ~recursive_by_default:false rf
            | Some prev_vb ->
              token_between prev_vb lhs AND, None
          in
          let kw = Keyword.decorate token ~extension:None attrs ~later:lhs in
          match modifier with
          | None -> kw
          | Some tok ->
            let modif = token_between kw lhs tok in
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
          text, { mb with pmb_attributes = attrs }
        in
        let binding = Module_binding.pp mb in
        let keyword =
          let lhs = binding.name in
          let token, modifier =
            match !previous_mb with
            | None ->
              token_before ~start:loc.Location.loc_start lhs MODULE,
              rec_token ~recursive_by_default:false rf
            | Some prev_mb ->
              token_between prev_mb lhs AND, None
          in
          let kw = Keyword.decorate token ~extension:None [] ~later:lhs in
          match modifier with
          | None -> kw
          | Some tok ->
            let modif = token_between kw lhs tok in
            kw ^/^ modif
        in
        let binding =
          Binding.Module.pp ~context:Struct ~keyword (Module_binding.pp mb)
        in
        previous_mb := Some binding;
        Attribute.prepend_text text binding
      ) mbs
    in
    separate (twice hardline) (List.hd mbs) (List.tl mbs)

  let pp_module ~loc mb = pp_modules ~loc Nonrecursive [ mb ]
  let pp_recmodule ~loc mbs = pp_modules ~loc Recursive mbs

  let pp_include { pincl_mod; pincl_attributes; pincl_loc } =
    let incl = Module_expr.pp pincl_mod in
    let kw =
      let loc = { pincl_loc with loc_end = incl.loc.loc_start } in
      string ~loc "include"
    in
    Attribute.attach_to_top_item
      (group (kw ^/^ incl))
      pincl_attributes

  let pp_extension ext attrs =
    let ext = Extension.pp Structure_item ext in
    Attribute.attach_to_top_item ext attrs

  let pp_item ?(first=false) ({ pstr_desc; pstr_loc = loc; _ } as _item) =
    match pstr_desc with
    | Pstr_eval (e, attrs) -> pp_eval ~first e attrs
    | Pstr_value (rf, vbs) -> pp_value ~loc rf vbs
    | Pstr_primitive vd -> Value_description.pp vd
    | Pstr_type (rf, tds) -> Type_declaration.pp_decl rf tds
    | Pstr_typext te -> Type_extension.pp te
    | Pstr_exception exn -> Type_exception.pp exn
    | Pstr_module mb -> pp_module ~loc mb
    | Pstr_recmodule mbs -> pp_recmodule ~loc mbs
    | Pstr_modtype mtd -> Module_type_declaration.pp mtd
    | Pstr_open od -> Open_declaration.pp Attached_to_structure_item od
    | Pstr_class cds -> Class_declaration.pp cds
    | Pstr_class_type ctds -> Class_type_declaration.pp ctds
    | Pstr_include incl -> pp_include incl
    | Pstr_attribute attr -> Attribute.pp Free_floating attr
    | Pstr_extension (ext, attrs) -> pp_extension ext attrs

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
    | Pstr_attribute _, Pstr_attribute _
    | Pstr_extension _, Pstr_extension _ -> true
    | _ -> false

  let pp_nonempty i is =
    match
      group_by_desc [ i ] is
      |> List.map (List.map pp_item)
      |> List.map collate_toplevel_items
    with
    | [] -> assert false
    | [ doc ] -> doc
    | doc :: docs -> separate (twice hardline) doc docs
end

and Signature : sig
  val pp_nonempty : signature_item -> signature -> document
end = struct
  let pp_extension ext attrs =
    let ext = Extension.pp Structure_item ext in
    Attribute.attach_to_top_item ext attrs

  let pp_include { pincl_mod; pincl_attributes; pincl_loc } =
    let incl = Module_type.pp pincl_mod in
    let kw =
      let loc = { pincl_loc with loc_end = incl.loc.loc_start } in
      string ~loc "include"
    in
    Attribute.attach_to_top_item
      (group (kw ^/^ incl))
      pincl_attributes

  let pp_recmodules mds =
    let mds =
      let i = ref 0 in
      List.concat_map (fun md ->
        let text, md =
          let text, attrs =
            Attribute.extract_text md.pmd_attributes
              ~item_start_pos:md.pmd_loc.loc_start
          in
          text, { md with pmd_attributes = attrs }
        in
        let keyword = if !i = 0 then "module rec" else "and" in
        incr i;
        let keyword =
          let loc = { md.pmd_loc with loc_end = md.pmd_name.loc.loc_start } in
          string ~loc keyword
        in
        let binding =
          Binding.Module.pp (Module_declaration.pp_raw md)
            ~context:(Module_declaration.decide_context md.pmd_type) ~keyword
        in
        Attribute.prepend_text text binding
      ) mds
    in
    separate (twice hardline) (List.hd mds) (List.tl mds)

  let pp_item ({ psig_desc; _ } as _item) =
    match psig_desc with
    | Psig_value vd -> Value_description.pp vd
    | Psig_type (rf, decls) -> Type_declaration.pp_decl rf decls
    | Psig_typesubst decls -> Type_declaration.pp_subst decls
    | Psig_typext te -> Type_extension.pp te
    | Psig_exception exn -> Type_exception.pp exn
    | Psig_module md -> Module_declaration.pp md
    | Psig_recmodule pmds -> pp_recmodules pmds
    | Psig_modsubst ms -> Module_substitution.pp ms
    | Psig_modtype mtd -> Module_type_declaration.pp mtd
    | Psig_open od -> Open_description.pp od
    | Psig_include incl -> pp_include incl
    | Psig_attribute attr -> Attribute.pp Free_floating attr
    | Psig_extension (ext, attrs) -> pp_extension ext attrs
    | Psig_class cds -> Class_description.pp cds
    | Psig_class_type ctds -> Class_type_declaration.pp ctds

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
    | Psig_class_type _, Psig_class_type _ -> true
    | _ -> false

  let pp_nonempty i is =
    match
      group_by_desc [ i ] is
      |> List.map (List.map pp_item)
      |> List.map collate_toplevel_items
    with
    | [] -> assert false
    | [ doc ] -> doc
    | doc :: docs -> separate (twice hardline) doc docs
end

and Value_description : sig
  val pp : value_description -> document
end = struct
  let pp vd =
    let name = str vd.pval_name in
    let ctyp = Core_type.pp [] vd.pval_type in
    let kw_string, with_prim =
      match vd.pval_prim with
      | [] -> "val", ctyp
      | p :: ps ->
        let prims =
          separate_map (break 1) ~f:(fun {loc; txt} ->
            dquotes (Constant.pp_string_lit ~loc txt)) p ps
        in
        let equals = token_between ctyp prims EQUAL in
        "external", ctyp ^^ break_before (group (equals ^/^ prims))
    in
    let kw =
      let loc = { vd.pval_loc with loc_end = name.loc.loc_start } in
      string ~loc kw_string
    in
    let colon = token_between name with_prim COLON in
    let doc =
      prefix ~indent:2 ~spaces:1 (group (kw ^/^ name))
        (concat colon with_prim ~sep:PPrint.(ifflat space (twice space)))
    in
    Attribute.attach_to_top_item doc vd.pval_attributes
end

and Type_extension : sig
  val pp : type_extension -> document
end = struct
  let constructors = function
    | [] -> assert false
    | c :: cs ->
      let cstrs =
        separate_map PPrint.(break 1 ^^ bar ^^ space)
          ~f:Constructor_decl.pp_extension c cs
      in
      (* FIXME *)
      let prefix =
        let open PPrint in
        ifflat empty (bar ^^ space)
      in
      prefix ++ cstrs

  let pp { ptyext_path; ptyext_params; ptyext_constructors; ptyext_private;
           ptyext_attributes; ptyext_loc } =
    let path = Longident.pp ptyext_path in
    let lhs = Type_declaration.with_params ptyext_params path in
    let constructors = constructors ptyext_constructors in
    let rhs =
      match ptyext_private with
      | None -> constructors
      | Some loc -> group (string ~loc "private" ^/^ constructors)
    in
    let rhs = Attribute.attach_to_top_item rhs ptyext_attributes in
    let keyword =
      let loc = { ptyext_loc with loc_end = lhs.loc.loc_start } in
      string ~loc "type"
    in
    Binding.pp_simple ~keyword ~binder:PLUSEQ lhs rhs
end

and Type_exception : sig
  val pp : type_exception -> document
end = struct
  let pp { ptyexn_constructor; ptyexn_attributes; ptyexn_loc } =
    let cstr = Constructor_decl.pp_extension ptyexn_constructor in
    let kw =
      let loc = { ptyexn_loc with loc_end = cstr.loc.loc_start } in
      string ~loc "exception"
    in
    let doc = group (prefix ~spaces:1 ~indent:2 kw cstr) in
    Attribute.attach_to_top_item doc ptyexn_attributes
end

and Type_declaration : sig
(*   val pp : type_declaration -> document * document *)

  val with_params
    :  ?always_enclosed:bool
    -> ?enclosing:(document -> document)
    -> (core_type * variance) list
    -> document
    -> document

  type keyword =
    | String_prefix of string
    | Formatted of document

  val pp_decl : rec_flag -> type_declaration list -> document

  val pp_subst : type_declaration list -> document

  val pp_with_constraint
    :  ?override_name:document
    -> ?binder:Source_parsing.Parser.token
    -> keyword:keyword
    -> type_declaration
    -> document
end = struct
  let pp_param (ct, var) =
    let ct = Core_type.pp [] ct in
    match var with
    | Invariant -> ct
    | Covariant -> plus ++ ct
    | Contravariant -> minus ++ ct

  let with_params ?(always_enclosed=false) ?(enclosing=parens) lst name =
    match lst with
    | [] -> name
    | [ x ] when always_enclosed -> group (enclosing (pp_param x) ^/^ name)
    | [ x ] -> group (pp_param x ^/^ name)
    | x :: xs ->
      let params = separate_map PPrint.(comma ^^ break 1) ~f:pp_param x xs in
      group (enclosing params ^/^ name)

  let label_declaration { pld_name; pld_mutable; pld_type; pld_attributes; _ } =
    let name = str pld_name in
    let typ  = Core_type.pp [] pld_type in
    let colon = token_between name typ COLON in
    let lhs = group (name ^/^ colon) in
    let with_mutable_ =
      match pld_mutable with
      | Mutable loc -> group (string ~loc "mutable" ^/^ lhs)
      | Immutable -> lhs
    in
    let decl = group (nest 2 (with_mutable_ ^/^ typ)) in
    decl, List.map (Attribute.pp Attached_to_item) pld_attributes

  let record lbl_decls =
    (* FIXME: loc won't be use since the list is nonempty *)
    let fields = List.map label_declaration lbl_decls in
    Record_like.pp ~loc:Location.none
      ~formatting:Fit_or_vertical (* never wrap decls *)
      ~left:lbrace
      ~right:rbrace
      fields

  let () = Constructor_decl.pp_record := record

  let variant { Location.loc; txt = cstrs } =
    match cstrs with
    | [] -> string ~loc "|"
    | cstr :: cstrs ->
      let cstrs =
        separate_map PPrint.(break 1 ^^ bar ^^ space)
          ~f:(fun c -> nest 2 (Constructor_decl.pp_decl c))
          cstr cstrs
      in
      let prefix =
        let open PPrint in
        ifflat empty (bar ^^ space)
      in
      (* FIXME: ++ :| *)
      prefix ++ cstrs

  let non_abstract_kind = function
    | Ptype_abstract -> assert false
    | Ptype_open loc -> string ~loc ".."
    | Ptype_record lbl_decls -> record lbl_decls
    | Ptype_variant cstrs -> variant cstrs

  let pp_constraint (ct1, ct2, _) =
    let ct1 = Core_type.pp [] ct1 in
    let ct2 = Core_type.pp [] ct2 in
    let equals = token_between ct1 ct2 EQUAL in
    ct1 ^/^ equals ^/^ ct2

  let add_constraints decl = function
    | [] -> decl
    | cstr :: cstrs ->
      let cstrs =
        separate_map (PPrint.break 1) ~f:pp_constraint cstr cstrs
      in
      let kw = token_between decl cstrs CONSTRAINT in
      prefix ~indent:2 ~spaces:1 decl
        (kw ^/^ hang 2 (break_before ~spaces:0 cstrs))

  type keyword =
    | String_prefix of string
    | Formatted of document

  let pp ?override_name ?binder ~keyword manifest_ps
      { ptype_name; ptype_params; ptype_cstrs; ptype_kind; ptype_private;
        ptype_manifest; ptype_attributes; ptype_loc } =
    let name = Option.value override_name ~default:(str ptype_name) in
    let lhs = with_params ptype_params name in
    let manifest_opt = Option.map (Core_type.pp manifest_ps) ptype_manifest in
    let rhs =
      (* I didn't know how to express this nightmare more cleanly. *)
      match manifest_opt, ptype_private, ptype_kind with
      | None, None, Ptype_abstract ->
          None
      | Some manifest, None, Ptype_abstract ->
          Some manifest
      | Some manifest, Some loc, Ptype_abstract ->
          Some (group (string ~loc "private" ^/^ manifest))
      | Some manifest, None, kind ->
          let kind = non_abstract_kind kind in
          let equals = token_between manifest kind EQUAL in
          Some (manifest ^/^ equals ^/^ kind)
      | Some manifest, Some loc, kind ->
          let private_ = string ~loc "private" in
          let equals = token_between manifest private_ EQUAL in
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
      match keyword with
      | String_prefix keyword ->
        let loc = { ptype_loc with loc_end = lhs.loc.loc_start } in
        string ~loc keyword
      | Formatted doc ->
        doc
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

  let rec_flag = function
    | Recursive -> ""
    | Nonrecursive -> " nonrec"

  let pp_with_constraint ?override_name ?binder ~keyword td =
    pp ?override_name ?binder ~keyword [ Printing_stack.With_constraint ] td

  let pp_decl rf decls =
    let decls =
      let i = ref 0 in
      List.concat_map (fun decl ->
        let text, decl =
          let text, attrs =
            Attribute.extract_text decl.ptype_attributes
              ~item_start_pos:decl.ptype_loc.loc_start
          in
          text, { decl with ptype_attributes = attrs }
        in
        let keyword =
          String_prefix (if !i = 0 then "type" ^ rec_flag rf else "and")
        in
        incr i;
        let binding = pp ~keyword [] decl in
        Attribute.prepend_text text binding
      ) decls
    in
    separate (twice hardline) (List.hd decls) (List.tl decls)

  let pp_subst decls =
    let decls =
      let i = ref 0 in
      List.concat_map (fun decl ->
        let text, decl =
          let text, attrs =
            Attribute.extract_text decl.ptype_attributes
              ~item_start_pos:decl.ptype_loc.loc_start
          in
          text, { decl with ptype_attributes = attrs }
        in
        let keyword = String_prefix (if !i = 0 then "type" else "and") in
        incr i;
        let binding = pp ~binder:COLONEQUAL ~keyword [] decl in
        Attribute.prepend_text text binding
      ) decls
    in
    separate hardline (List.hd decls) (List.tl decls)
end

and Class_type : sig
  val pp : class_type -> document

  val pp_constr : Longident.t -> core_type list -> document
end = struct
  let pp_constr name args =
    let name = Longident.pp name in
    match args with
    | [] -> name
    | x :: xs ->
      let break_after =
        let rec ends_in_obj = function
          | [] -> assert false
          | [ x ] -> Core_type.ends_in_obj x
          | _ :: xs -> ends_in_obj xs
        in
        if ends_in_obj args
        then break_after ~spaces:1
        else Fun.id
      in
      let break_before =
        if Core_type.starts_with_obj x
        then break_before ~spaces:1
        else Fun.id
      in
      let args =
        group (
          brackets (
            break_before @@ break_after @@
            separate_map PPrint.(comma ^^ break 1) ~f:(Core_type.pp []) x xs
          )
        )
      in
      args ^/^ name

  let rec pp { pcty_desc; pcty_loc; pcty_attributes } =
    let doc, attrs = pp_desc pcty_loc pcty_desc pcty_attributes in
    Attribute.attach_to_item doc attrs

  and pp_open ~loc od ct attrs =
    let od = Open_description.pp ~extra_attrs:attrs od in
    let ct = pp ct in
    let in_ = token_between od ct IN in
    let let_ =
      let loc = { loc with Location.loc_end = od.loc.loc_start } in
      string ~loc "let"
    in
    group (let_ ^/^ od ^/^ in_) ^/^ ct

  and pp_arrow lbl ct cty =
    let param =
      (* Fake printing stack :/ *)
      let ps = [ Printing_stack.Core_type (Ptyp_arrow ([], ct)) ] in
      Core_type.pp_param ps (lbl, ct)
    in
    let cty = pp cty in
    let arrow = token_between param cty MINUSGREATER in
    param ^/^ group (arrow ^/^ cty)

  and pp_desc loc desc attrs =
    match desc with
    | Pcty_constr (ct, args) -> pp_constr ct args, attrs
    | Pcty_signature sg -> Class_signature.pp ~loc sg, attrs
    | Pcty_arrow (lbl, ct, cty) -> pp_arrow lbl ct cty, attrs
    | Pcty_extension ext -> Extension.pp Item ext, attrs
    | Pcty_open (od, ct) -> pp_open ~loc od ct attrs, []
end

and Class_expr : sig
  val pp : Printing_stack.t -> class_expr -> document
end = struct
  (* TODO: much of this is just copy pasted from Expression; factorize. *)

  let rec pp ps { pcl_desc; pcl_loc; pcl_attributes } =
    let ps, enclose =
      if Attribute.has_non_doc pcl_attributes then
        Printing_stack.parenthesize (Attribute :: ps)
      else
        ps, Fun.id
    in
    let ps = Printing_stack.Class_expr pcl_desc :: ps in
    let desc = group (pp_desc ~loc:pcl_loc ps pcl_desc) in
    let doc = Attribute.attach_to_item desc pcl_attributes in
    enclose doc

  and pp_fun ps ~loc params ce =
    let params =
      separate_map (PPrint.break 1) ~f:Fun_param.pp
        (List.hd params) (List.tl params)
    in
    let ps, enclose = Printing_stack.parenthesize ps in
    let body = pp ps ce in
    (* FIXME: copied from expressions. factorize. *)
    let fun_ =
      let loc = { loc with Location.loc_end = params.loc.loc_start } in
      string ~loc "fun"
    in
    let arrow = token_between params body MINUSGREATER in
    prefix ~indent:2 ~spaces:1
      (group ((prefix ~indent:2 ~spaces:1 fun_ params) ^/^ arrow))
      body
    |> enclose

  and pp_apply ps ce = function
    | [] -> assert false (* can't apply without args! *)
    | arg :: args ->
      let ps, enclose = Printing_stack.parenthesize ps in
      let ce = pp ps ce in
      Application.pp_simple ps ce arg args
      |> enclose

  and pp_let ~loc ps rf vbs ce =
    let vbs =
      let previous_vb = ref None in
      List.concat_map (fun vb ->
        let text, vb =
          let text, attrs =
            Attribute.extract_text vb.pvb_attributes
              ~item_start_pos:vb.pvb_loc.loc_start
          in
          text, { vb with pvb_attributes = attrs }
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
              token_before ~start:loc.Location.loc_start lhs LET,
              rec_token ~recursive_by_default:false rf
            | Some prev_vb ->
              token_between prev_vb lhs AND, None
          in
          let kw = Keyword.decorate token ~extension:None attrs ~later:lhs in
          match modifier with
          | None -> kw
          | Some tok ->
            let modif = token_between kw lhs tok in
            kw ^/^ modif
        in
        let binding = Binding.pp ~keyword binding in
        previous_vb := Some binding;
        Attribute.prepend_text text binding
      ) vbs
    in
    let vbs = separate hardline (List.hd vbs) (List.tl vbs) in
    let ps, enclose = Printing_stack.parenthesize ps in
    let ce = pp ps ce in
    let in_ = token_between vbs ce IN in
    group (vbs ^/^ in_) ^^ hardline ++ ce
    |> enclose

  and pp_constraint ps ce ct =
    let ce = pp ps ce in
    let ct = Class_type.pp ct in
    let colon = token_between ce ct COLON in
    group (parens (ce ^/^ colon ^/^ ct))

  and pp_open ps ~loc od ce =
    let od = Open_description.pp od in
    let ps, enclose = Printing_stack.parenthesize ps in
    let ce = pp ps ce in
    let in_ = token_between od ce IN in
    let let_ =
      let loc = { loc with Location.loc_end = od.loc.loc_start } in
      string ~loc "let"
    in
    group (let_ ^/^ od ^/^ in_) ^/^ ce
    |> enclose

  and pp_desc ps ~loc = function
    | Pcl_constr (name, args) -> Class_type.pp_constr name args
    | Pcl_structure str -> Class_structure.pp ~loc str
    | Pcl_fun (params, ce) -> pp_fun ps ~loc params ce
    | Pcl_apply (ce, args) -> pp_apply ps ce args
    | Pcl_let (rf, vbs, ce) -> pp_let ~loc ps rf vbs ce
    | Pcl_constraint (ce, ct) -> pp_constraint ps ce ct
    | Pcl_extension ext -> Extension.pp Item ext
    | Pcl_open (od, ce) -> pp_open ps ~loc od ce
end

and Class_structure : sig
  val pp
    :  loc:Location.t
    -> ?ext_attrs:string loc option * attributes
    -> class_structure
    -> document

  val pp_constraint : loc:Location.t -> core_type -> core_type -> document
end = struct
  let pp_inherit ~loc override ce alias =
    let pre =
      let ce = Class_expr.pp [] ce in
      let inh_kw = token_before ~start:loc.Location.loc_start ce INHERIT in
      group (
        match override with
        | Override ->
          let bang = token_between inh_kw ce BANG in
          inh_kw ^^ bang ^/^ ce
        | _ -> inh_kw ^/^ ce
      )
    in
    match alias with
    | None -> pre
    | Some name ->
      let name = str name in
      let as_ = token_between pre name AS in
      group (pre ^/^ as_ ^/^ name)

  let pp_virtual ~start kind name mod_tok ct =
    let name = str name in
    let ct = Core_type.pp [] ct in
    let keyword =
      let kw = token_before ~start name kind in
      let virt = token_between kw name VIRTUAL in
      group (
        match mod_tok with
        | None -> kw ^/^ virt
        | Some tok ->
          let tok = token_between kw name tok in
          kw ^/^ merge_possibly_swapped ~sep:(PPrint.break 1) tok virt
      )
    in
    Binding.pp_simple ~binder:COLON ~keyword name ct

  let pp_concrete ~start kind name mod_tok override params
      (constr, coerce) expr =
    let name = str name in
    let keyword =
      let kw = token_before ~start name kind in
      let with_bang =
        match override with
        | Override -> kw ^^ token_between kw name BANG
        | _ -> kw
      in
      group (
        match mod_tok with
        | None -> with_bang
        | Some tok -> with_bang ^/^ token_between with_bang name tok
      )
    in
    let params = List.map Fun_param.pp params in
    let constr = Option.map (Core_type.pp []) constr in
    let coerce = Option.map (Core_type.pp []) coerce in
    let rhs = Some (Expression.pp [] expr) in
    let params =
      let loc = { name.loc with loc_start = name.loc.loc_end } in
      { txt = params; loc }
    in
    Binding.pp ~keyword { lhs = name; params; constr; coerce; rhs }

  let pp_field_kind ~loc:{ Location.loc_start; _ } kind name mod_tok = function
    | Cfk_virtual ct -> pp_virtual ~start:loc_start kind name mod_tok ct
    | Cfk_concrete (override, params, cts, expr) ->
      pp_concrete ~start:loc_start kind name mod_tok override params cts expr

  let pp_val ~loc name mut cfk =
    let modifier_token =
      match mut with
      | Immutable -> None
      | Mutable _ -> Some Source_parsing.Parser.MUTABLE
    in
    pp_field_kind ~loc VAL name modifier_token cfk

  let pp_method ~loc name priv cfk =
    let modifier_token =
      match priv with
      | Public -> None
      | Private -> Some Source_parsing.Parser.PRIVATE
    in
    pp_field_kind ~loc METHOD name modifier_token cfk

  let pp_constraint ~loc:{ Location.loc_start; _ } ct1 ct2 =
    let ct1 = Core_type.pp [] ct1 in
    let ct2 = Core_type.pp [] ct2 in
    let keyword = token_before ~start:loc_start ct1 CONSTRAINT in
    Binding.pp_simple ~keyword ct1 ct2

  let pp_init ~loc:{ Location.loc_start; _ } expr =
    let expr = Expression.pp [] expr in
    let init = token_before ~start:loc_start expr INITIALIZER in
    group (init ^/^ expr)

  let pp_field_desc ~loc = function
    | Pcf_inherit (override, ce, alias) -> pp_inherit ~loc override ce alias
    | Pcf_val (name, mut, cf) -> pp_val ~loc name mut cf
    | Pcf_method (name, priv, cf) -> pp_method ~loc name priv cf
    | Pcf_constraint (ct1, ct2) -> pp_constraint ~loc ct1 ct2
    | Pcf_initializer e -> pp_init ~loc e
    | Pcf_attribute attr -> Attribute.pp Free_floating attr
    | Pcf_extension ext -> Extension.pp Structure_item ext

  let pp_field { pcf_desc; pcf_loc; pcf_attributes } =
    let doc = pp_field_desc ~loc:pcf_loc pcf_desc in
    Attribute.attach_to_top_item doc pcf_attributes

  let pp ~(loc:Location.t) ?ext_attrs:(extension, attrs = None, [])
      { pcstr_self; pcstr_fields } =
    let obj_with_self =
      match pcstr_self.ppat_desc with
      | Ppat_any -> (* no self *)
        let kw = string ~loc:pcstr_self.ppat_loc "object" in
        let later =
          (* We don't know what comes next yet! *)
          { txt = (); loc = { loc with loc_start = loc.loc_end }}
        in
        Keyword.decorate kw ~extension attrs ~later
      | _ ->
        let self = Pattern.pp [] pcstr_self in
        let obj =
          Keyword.decorate (token_before ~start:loc.loc_start self OBJECT)
            ~extension attrs ~later:self
        in
        group (obj ^/^ parens self)
    in
    match pcstr_fields with
    | [] ->
      let end_ = token_after ~stop:loc.loc_end obj_with_self END in
      obj_with_self ^/^ end_
    | f :: fs ->
      let fields = separate_map PPrint.(twice hardline) ~f:pp_field f fs in
      let end_ = token_after ~stop:loc.loc_end fields END in
      group (
        obj_with_self ^^ (nest 2 (break_before fields)) ^/^ end_
      )
end

and Class_signature : sig
  val pp : loc:Location.t -> class_signature -> document
end = struct
  let pp_inherit ~loc ct =
    let ct = Class_type.pp ct in
    let inh_kw = token_before ~start:loc.Location.loc_start ct INHERIT in
    group (inh_kw ^/^ ct)

  let pp_maybe_virtual ~start kind name mod_tok vf ct =
    let name = str name in
    let ct = Core_type.pp [] ct in
    let keyword =
      let kw = token_before ~start name kind in
      group (
        match mod_tok, vf with
        | None, Concrete -> kw
        | Some tok, Concrete ->
          let tok = token_between kw name tok in
          kw ^/^ tok
        | None, Virtual ->
          let virt = token_between kw name VIRTUAL in
          kw ^/^ virt
        | Some tok, Virtual ->
          let virt = token_between kw name VIRTUAL in
          let tok = token_between kw name tok in
          kw ^/^ merge_possibly_swapped ~sep:(PPrint.break 1) tok virt
      )
    in
    Binding.pp_simple ~binder:COLON ~keyword name ct

  let pp_val ~loc:{ Location.loc_start; _ } (name, mut, vf, ct) =
    let mod_tok =
      match mut with
      | Immutable -> None
      | Mutable _ -> Some Source_parsing.Parser.MUTABLE
    in
    pp_maybe_virtual ~start:loc_start VAL name mod_tok vf ct

  let pp_method ~loc:{ Location.loc_start; _ } (name, priv, vf, ct) =
    let mod_tok =
      match priv with
      | Public -> None
      | Private -> Some Source_parsing.Parser.PRIVATE
    in
    pp_maybe_virtual ~start:loc_start METHOD name mod_tok vf ct

  let pp_field_desc ~loc = function
    | Pctf_inherit ct -> pp_inherit ~loc ct
    | Pctf_val val_ -> pp_val ~loc val_
    | Pctf_method meth -> pp_method ~loc meth
    | Pctf_constraint (ct1, ct2) -> Class_structure.pp_constraint ~loc ct1 ct2
    | Pctf_attribute attr -> Attribute.pp Free_floating attr
    | Pctf_extension ext -> Extension.pp Structure_item ext

  let pp_field { pctf_desc; pctf_loc; pctf_attributes } =
    let doc = pp_field_desc ~loc:pctf_loc pctf_desc in
    Attribute.attach_to_item doc pctf_attributes

  let pp ~loc { pcsig_self = _; pcsig_fields } =
    (* FIXME *)
    match pcsig_fields with
    | [] -> string ~loc "object end"
    | f :: fs ->
      let fields = separate_map PPrint.(twice hardline) ~f:pp_field f fs in
      (* FIXME *)
      group (
        enclose ~before:!^"object" ~after:PPrint.(break 1 ^^ !^"end")
          (nest 2 (break_before fields))
      )


end

and Class_declaration : sig
  val pp : class_declaration list -> document
end = struct
  let pp cds =
    let cds =
      let previous_cd = ref None in
      List.concat_map (fun cd ->
        let { pci_virt; pci_params; pci_name; pci_term_params; pci_type;
              pci_expr; pci_loc; pci_attributes } = cd in
        let text, pci_attributes =
          Attribute.extract_text pci_attributes
              ~item_start_pos:pci_loc.loc_start
        in
        let lhs =
          Type_declaration.with_params ~always_enclosed:true ~enclosing:brackets
            pci_params (str pci_name)
        in
        let binding =
          { Binding.lhs;
            params =
              (let loc = { lhs.loc with loc_start = lhs.loc.loc_end } in
              { loc; txt = List.map Fun_param.pp pci_term_params });
            constr = Option.map Class_type.pp pci_type;
            coerce = None;
            rhs = Some (Class_expr.pp [] pci_expr) }
        in
        let keyword =
          match !previous_cd with
          | None -> token_before ~start:pci_loc.loc_start lhs CLASS
          | Some cd -> token_between cd lhs AND
        in
        let keyword =
          match pci_virt with
          | Concrete -> keyword
          | Virtual ->
            let virt = token_between keyword lhs VIRTUAL in
            group (keyword ^/^ virt)
        in
        let doc = Binding.pp binding ~keyword in
        previous_cd := Some doc;
        Attribute.prepend_text text @@
        Attribute.attach_to_top_item doc pci_attributes
      ) cds
    in
    separate PPrint.(twice hardline) (List.hd cds) (List.tl cds)
end

and Class_description : sig
  val pp : class_description list -> document
end = struct
  let pp cds =
    let cds =
      let previous_cd = ref None in
      List.concat_map (fun cd ->
        let { pci_virt; pci_params; pci_name; pci_term_params; pci_type;
              pci_expr; pci_loc; pci_attributes } = cd in
        let text, pci_attributes =
          Attribute.extract_text pci_attributes
            ~item_start_pos:pci_loc.loc_start
        in
        let lhs =
          Type_declaration.with_params ~always_enclosed:true ~enclosing:brackets
            pci_params (str pci_name)
        in
        assert (Option.is_none pci_type);
        let binding =
          { Binding.lhs;
            params =
              (let loc = { lhs.loc with loc_start = lhs.loc.loc_end } in
              { loc; txt = List.map Fun_param.pp pci_term_params });
            constr = None;
            coerce = None;
            rhs = Some (Class_type.pp pci_expr) }
        in
        let keyword =
          match !previous_cd with
          | None -> token_before ~start:pci_loc.loc_start lhs CLASS
          | Some cd -> token_between cd lhs AND
        in
        let keyword =
          match pci_virt with
          | Concrete -> keyword
          | Virtual ->
            let virt = token_between keyword lhs VIRTUAL in
            group (keyword ^/^ virt)
        in
        let doc = Binding.pp binding ~binder:COLON ~keyword in
        previous_cd := Some doc;
        Attribute.prepend_text text @@
        Attribute.attach_to_top_item doc pci_attributes
      ) cds
    in
    separate PPrint.(twice hardline) (List.hd cds) (List.tl cds)
end

and Class_type_declaration : sig
  val pp : class_description list -> document
end = struct
  let pp cds =
    let cds =
      let previous_cd = ref None in
      List.concat_map (fun cd ->
        let { pci_virt; pci_params; pci_name; pci_term_params; pci_type;
              pci_expr; pci_loc; pci_attributes } = cd in
        let text, pci_attributes =
          Attribute.extract_text pci_attributes
            ~item_start_pos:pci_loc.loc_start
        in
        let lhs =
          Type_declaration.with_params ~always_enclosed:true ~enclosing:brackets
            pci_params (str pci_name)
        in
        assert (Option.is_none pci_type);
        let binding =
          { Binding.lhs;
            params =
              (let loc = { lhs.loc with loc_start = lhs.loc.loc_end } in
              { loc; txt = List.map Fun_param.pp pci_term_params });
            constr = None;
            coerce = None;
            rhs = Some (Class_type.pp pci_expr) }
        in
        let keyword =
          match !previous_cd with
          | Some cd -> token_between cd lhs AND
          | None ->
            let class_ = token_before ~start:pci_loc.loc_start lhs CLASS in
            let type_ = token_between class_ lhs TYPE in
            group (class_ ^/^ type_)
        in
        let keyword =
          match pci_virt with
          | Concrete -> keyword
          | Virtual ->
            let virt = token_between keyword lhs VIRTUAL in
            group (keyword ^/^ virt)
        in
        let doc = Binding.pp binding ~keyword in
        previous_cd := Some doc;
        Attribute.prepend_text text @@
        Attribute.attach_to_top_item doc pci_attributes
      ) cds
    in
    separate PPrint.(twice hardline) (List.hd cds) (List.tl cds)
end

and Open_description : sig
  val pp : ?extra_attrs:attributes -> open_description -> document
end = struct
  let pp ?(extra_attrs=[])
      { popen_expr; popen_override; popen_attributes; popen_loc } =
    let expr = Longident.pp popen_expr in
    let kw =
      let loc = { popen_loc with loc_end = expr.loc.loc_start } in
      string ~loc
        (match popen_override with
         | Override -> "open!"
         | _ -> "open")
    in
    let opn = group (Attribute.attach_to_item kw extra_attrs ^/^ expr) in
    Attribute.attach_to_top_item opn popen_attributes
end

and Open_declaration : sig
  val pp : ?ext_attrs:string loc option * attributes -> Attribute.kind ->
    open_declaration -> document
end = struct
  let pp ?ext_attrs:(extension, attrs = None, []) kind
      { popen_expr; popen_override; popen_attributes; popen_loc } =
    let expr = Module_expr.pp popen_expr in
    let kw =
      let loc = { popen_loc with loc_end = expr.loc.loc_start } in
      string ~loc
        (match popen_override with
         | Override -> "open!"
         | _ -> "open")
    in
    let kw = Keyword.decorate kw ~extension attrs ~later:expr in
    let opn = group (kw ^/^ expr) in
    Attribute.attach kind opn popen_attributes
end

let interface sg =
  let doc =
    match sg with
    | [] -> empty ~loc:Location.none
    | si :: sg -> Signature.pp_nonempty si sg
  in
  Document.attach_surrounding_comments doc

let implementation str =
  let doc =
    match str with
    | [] -> empty ~loc:Location.none
    | si :: st -> Structure.pp_nonempty si st
  in
  Document.attach_surrounding_comments doc
