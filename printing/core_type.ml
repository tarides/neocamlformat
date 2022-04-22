open Document
open Custom_combinators
open Import
open Asttypes
open Source_tree
open Location

module rec Polymorphic_variant : sig
  val pp_row
    :  loc:Location.t
    -> row_field list
    -> closed_flag
    -> label loc list option
    -> Document.t
end = struct
  let pp_tag (tag : string Location.loc) =
    string ~loc:tag.loc ("`" ^ tag.txt)

  module Row_field : sig
    val pp : row_field -> Document.t
  end = struct
    let pp_params p ps =
      List.fold_left (fun acc ct ->
        let ct = Core_type.pp ct in
        let ampersand = Token.pp ~after:acc ~before:ct AMPERSAND in
        acc ^/^ ampersand ^/^ ct
      ) (Core_type.pp p) ps

    let pp_desc = function
      | Rinherit ct -> Core_type.pp ct
      | Rtag (tag, _, []) -> pp_tag tag
      | Rtag (tag, has_empty_constr, p :: ps) ->
        let tag = pp_tag tag in
        let params = pp_params p ps in
        let params =
          if not has_empty_constr then
            params
          else
            let amper = Token.pp ~after:tag ~before:params AMPERSAND in
            amper ^/^ params
        in
        Two_separated_parts.sep_with_first tag params ~sep:OF

    let pp { prf_desc; prf_attributes; _ } =
      let desc = pp_desc prf_desc in
      Attribute.attach_to_item desc prf_attributes
  end


  (* Looks a lot like [left_assoc_map] … except for the first element...
     TODO: generalize a bit [left_assoc_map] and rename it to [dock] *)
  let dock_fields ~opening_token x xs =
    let fmt x = nest 2 (blank 1 ^^ x) in
    List.fold_left (fun acc elt ->
      let elt = fmt elt in
      let pipe = Token.pp ~after:acc ~before:elt BAR in
      acc ^/^ group (pipe ^^ elt)
    ) (group (opening_token ^^ fmt x)) xs

  let pp_row_prefix ~prefix_if_necessary ~loc ~opening_token x xs =
    let needs_prefix =
      prefix_if_necessary &&
        opening_token = Source_parsing.Parser.LBRACKET &&
          xs = [] &&
            (match x.prf_desc with
            | Rinherit _ -> true
            | _ -> false)
    in
    let x = Row_field.pp x in
    let xs = List.map Row_field.pp xs in
    let opening_token = Token.pp ~inside:loc ~before:x opening_token in
    let opening_token =
      if not needs_prefix then
        opening_token
      else
        let pipe = Token.pp ~after:opening_token ~before:x BAR in
        opening_token ^/^ pipe
    in
    dock_fields ~opening_token x xs

  let pp_simple_row ~loc ~opening_token ~hang_indent = function
    | [] ->
      let lbracket = Token.pp ~inside:loc opening_token in
      let rbracket = Token.pp ~inside:loc ~after:lbracket RBRACKET in
      group (lbracket ^/^ rbracket)
    | x :: xs ->
      let fields =
        pp_row_prefix ~prefix_if_necessary:true ~loc ~opening_token x xs
      in
      let rbracket = Token.pp ~inside:loc ~after:fields RBRACKET in
      hang hang_indent (fields ^/^ rbracket)

  let pp_mixed_row ~loc ~labels:(l, ls) = function
    | [] -> assert false (* always at least one field *)
    | x :: xs ->
      let fields =
        pp_row_prefix ~prefix_if_necessary:false ~loc
          ~opening_token:LBRACKETLESS x xs
      in
      let labels = flow_map (break 1) pp_tag l ls in
      let sep = Token.pp ~after:fields ~before:labels GREATER in
      let rbracket = Token.pp ~inside:loc ~after:labels RBRACKET in
      hang 1 (fields ^/^ sep ^/^ labels ^/^ rbracket)

  let pp_row ~loc fields closed present =
    match closed, present with
    | Open, Some _ -> assert false
    | Closed, None ->
      pp_simple_row ~loc ~opening_token:LBRACKET ~hang_indent:0 fields
    | Open, None ->
      pp_simple_row ~loc ~opening_token:LBRACKETGREATER ~hang_indent:1 fields
    | Closed, Some [] ->
      pp_simple_row ~loc ~opening_token:LBRACKETLESS ~hang_indent:1 fields
    | Closed, Some (l :: ls) -> pp_mixed_row ~loc ~labels:(l, ls) fields
end


and Core_type : sig
  val ends_in_obj : core_type -> bool
  val starts_with_obj : core_type -> bool
  val pp : core_type -> Document.t
  val pp_param : (arg_label * core_type) -> Document.t
end = struct
  let rec starts_with_obj core_type =
    match core_type.ptyp_desc with
    | Ptyp_alias (lhs, _)
    | Ptyp_tuple (lhs :: _)
    | Ptyp_arrow ((_, lhs) :: _, _)
      ->
      starts_with_obj lhs
    | Ptyp_object (_, _) -> true
    | Ptyp_any
    | Ptyp_var _
    | Ptyp_parens _
    | Ptyp_tuple _
    | Ptyp_arrow _
    | Ptyp_constr (_, _)
    | Ptyp_class (_, _)
    | Ptyp_variant (_, _, _)
    | Ptyp_poly _
    | Ptype_poly _
    | Ptyp_package _
    | Ptyp_extension _
      ->
      false

  let rec ends_in_obj core_type =
    core_type.ptyp_attributes = [] &&
      match core_type.ptyp_desc with
      | Ptyp_arrow (_, rhs) | Ptype_poly (_, rhs) | Ptyp_poly (_, rhs) ->
        ends_in_obj rhs
      | Ptyp_tuple lst -> ends_in_obj (List.hd (List.rev lst))
      | Ptyp_object (_, _) -> true
      | Ptyp_any
      | Ptyp_var _
      | Ptyp_parens _
      | Ptyp_constr (_, _)
      | Ptyp_class (_, _)
      | Ptyp_alias (_, _)
      | Ptyp_variant (_, _, _)
      | Ptyp_package _
      | Ptyp_extension _
        ->
        false

  let pp_var ~loc v =
    match String.index_opt v '\'' with
    | None -> string ~loc ("'" ^ v)
    | Some _ -> string ~loc ("' " ^ v)

  let rec pp
      { ptyp_loc; ptyp_desc; ptyp_attributes; ptyp_ext_attributes;
        ptyp_loc_stack = _ }
  =
    let doc =
      group (pp_desc ~loc:ptyp_loc ~ext_attrs:ptyp_ext_attributes ptyp_desc)
    in
    Attribute.attach_to_item doc ptyp_attributes

  and pp_desc ~loc ~ext_attrs = function
    | Ptyp_any -> underscore ~loc
    | Ptyp_var v -> pp_var ~loc v
    | Ptyp_parens ct -> parens ~loc (pp ct)
    | Ptyp_arrow (params, ct2) -> pp_arrow params ct2
    | Ptyp_tuple lst -> pp_tuple lst
    | Ptyp_constr (name, args) -> pp_constr ~loc name args
    | Ptyp_object (fields, closed) -> pp_object ~loc fields closed
    | Ptyp_class (name, args) -> pp_class ~loc name args
    | Ptyp_alias (ct, alias) -> pp_alias ct alias
    | Ptyp_variant (fields, closed, present) ->
      Polymorphic_variant.pp_row ~loc fields closed present
    | Ptyp_poly (vars, ct) -> pp_poly vars ct
    | Ptype_poly (vars, ct) -> pp_newtype_poly ~loc vars ct
    | Ptyp_package pkg -> pp_package ~loc ext_attrs pkg
    | Ptyp_extension ext -> Attribute.Extension.pp ~loc Item ext

  and pp_param (arg_label, ct) =
    let ct = hang 0 @@ pp ct in
    match arg_label with
    | Nolabel -> ct
    | Labelled { name; _ } ->
      let name = str name in
      let colon = Token.pp ~after:name ~before:ct COLON in
      prefix ~indent:2 ~spaces:0 (group (name ^^ colon)) ct
    | Optional { name; extra_info = `Single_token } ->
      prefix ~indent:2 ~spaces:0 (str name) ct
    | Optional { name; extra_info = `Previous_token loc } ->
      let question = string ~loc "?" in
      let name = str name in
      let colon = Token.pp ~after:name ~before:ct COLON in
      prefix ~indent:2 ~spaces:0 (group (question ^^ name ^^ colon)) ct

  and pp_arrow params res =
    let params =
      let fmt elt = hang 0 (pp_param elt) in
      List.fold_left (fun acc elt ->
        let elt = fmt elt in
        let sep = Token.pp ~after:acc ~before:elt MINUSGREATER in
        acc ^/^ group (sep ^^ blank 1 ^^ hang 0 elt)
      ) (fmt @@ List.hd params) (List.tl params)
    in
    let res = pp res in
    let arrow = Token.pp ~after:params ~before:res MINUSGREATER in
    let doc = params ^/^ group (arrow ^^ blank 1 ^^ hang 0 res) in
    doc

  and pp_tuple = function
    | [] -> assert false
    | x :: xs -> left_assoc_map ~sep:STAR ~f:pp x xs

  and pp_constr ~loc name args =
    let name = Longident.pp name in
    match args with
    | [] -> name
    | x :: xs -> pp_params ~loc x xs ^/^ name

  and pp_params ~loc first = function
    | [] -> pp first
    | rest ->
      let fmt elt = group (pp elt) in
      let params = tuple_fields fmt first rest in
      parens ~loc (hang 0 params)

  and pp_object ~loc fields closed =
    let fields = List.map Object_field.pp fields in
    let fields =
      match closed with
      | OClosed -> fields
      | OOpen loc -> fields @ [ string ~loc "..", [] ]
    in
    Record_like.pp ~loc ~formatting:Fit_or_vertical ~left:LESS ~right:GREATER
      fields

  and pp_class ~loc name args =
    let name = Longident.pp name in
    let hash = Token.pp ~inside:loc ~before:name HASH in
    match args with
    | [] -> hash ^^ name
    | x :: xs -> pp_params ~loc x xs ^/^ hash ^^ name

  and pp_alias ct alias =
    let ct = pp ct in
    let alias = pp_var ~loc:alias.loc alias.txt in
    let as_ = Token.pp ~after:ct ~before:alias AS in
    (* TODO: hang & ident one linebreak *)
    let doc = ct ^/^ as_ ^/^ alias in
    doc

  and pp_poly vars ct =
    (* FIXME: doesn't look right. *)
    let ct = pp ct in
    match vars with
    | [] -> ct
    | v :: vs ->
      let vars =
        separate_map space ~f:(fun v -> pp_var ~loc:v.loc v.txt) v vs
      in
      let dot = Token.pp ~after:vars ~before:ct DOT in
      prefix ~indent:2 ~spaces:1 (group (vars ^^ dot)) ct

  and pp_newtype_poly ~loc vars ct =
    (* FIXME: doesn't look right. *)
    let ct = pp ct in
    match vars with
    | [] -> ct
    | v :: vs ->
      let v = str v in
      let vs = List.map str vs in
      let type_ = Token.pp ~inside:loc ~before:v TYPE in
      let vars = separate space v vs in
      let dot = Token.pp ~after:vars ~before:ct DOT in
      prefix ~indent:2 ~spaces:1 (group (type_ ^/^ vars ^^ dot)) ct

  and pp_package ~loc (extension, attrs) pkg =
    let pkg = Package_type.pp pkg in
    let module_ =
      let tok = Token.pp ~inside:loc ~before:pkg MODULE in
      Keyword.decorate tok ~extension attrs
    in
    parens ~loc (module_ ^/^ pkg)

  let () =
    Attribute.Payload.(
      ct_ends_in_obj := ends_in_obj;
      pp_core_type := pp
    )
end


and Object_field : sig
  val pp : object_field -> Document.t * Document.t list
end = struct
  let pp_otag name ct =
    let name = str name in
    let ct = Core_type.pp ct in
    let colon = Token.pp ~after:name ~before:ct COLON in
    group (name ^^ colon) ^/^ ct

  let pp_desc = function
    | Otag (name, ct) -> pp_otag name ct
    | Oinherit ct -> Core_type.pp ct

  let pp { pof_desc; pof_attributes; _ } =
    let desc = pp_desc pof_desc in
    desc, List.map (Attribute.pp Attached_to_item) pof_attributes
end


and Package_type : sig
  val pp : package_type -> Document.t
end = struct
  let pp_constr (lid, ct) =
    let lid = Longident.pp lid in
    let ct = Core_type.pp ct in
    let colon = Token.pp ~after:lid ~before:ct EQUAL in
    lid ^/^ colon ^/^ ct

  let pp (lid, constrs) =
    let lid = Longident.pp lid in
    match constrs with
    | [] -> lid
    | x :: xs ->
      let constrs =
        List.fold_left (fun acc x ->
          let x = pp_constr x in
          let and_ = Token.pp ~after:acc ~before:x AND in
          let typ_ = Token.pp ~after:and_ ~before:x TYPE in
          acc ^/^ and_ ^/^ typ_ ^/^ x
        ) (pp_constr x) xs
      in
      let with_ = Token.pp ~after:lid ~before:constrs WITH in
      let type_ = Token.pp ~after:with_ ~before:constrs TYPE in
      group (lid ^/^ with_ ^/^ type_ ^/^ constrs)
end


include Core_type
