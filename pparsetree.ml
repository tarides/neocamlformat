open Asttypes
open Parsetree
open PPrint

let prefix ~indent:n ~spaces:b l r = prefix n b l r

module Tokens = struct
  let pipe = char '|'

  let as_ = string "as"

  let arrow = string "->"

  let module_ = string "module"

  let of_ = string "of"
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
  val pp : label loc -> document
end = struct
  let pp tag = bquote ^^ string tag.txt
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
    | Rtag (tag, true, []) -> Polymorphic_variant_tag.pp tag
    | Rtag (tag, has_empty_constr, params) ->
      let sep = break 1 ^^ ampersand ^^ break 1 in
      let params = separate_map sep Core_type.pp params in
      let params =
        if has_empty_constr then
          sep ^^ params
        else
          break 1 ^^ params
      in
      Polymorphic_variant_tag.pp tag ^/^ of_ ^^ params

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
