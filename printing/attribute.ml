open Document
open Import
open Source_tree

module Payload = struct
  let pp_struct : (structure_item -> structure_item list -> Document.t) ref =
    ref (fun _ _ -> assert false)

  let struct_ends_in_obj : (structure_item list -> bool) ref =
    ref (fun _ -> assert false)

  let pp_sig : (signature_item -> signature_item list -> Document.t) ref =
    ref (fun _ _ -> assert false)

  let sig_ends_in_obj : (signature_item list -> bool) ref =
    ref (fun _ -> assert false)

  let pp_core_type : (core_type -> Document.t) ref =
    ref (fun _ -> assert false)
  let ct_ends_in_obj : (core_type -> bool) ref =
    ref (fun _ -> assert false)
  let pp_expression : (expression -> Document.t) ref =
    ref (fun _ -> assert false)
  let pp_pattern : (pattern -> Document.t) ref =
    ref (fun _ -> assert false)

  let pstr tag = function
    | [] -> tag
    | si :: st as items ->
      let st = !pp_struct si st in
      let res = tag ^^ nest 2 (break_before st) in
      if !struct_ends_in_obj items then break_after res else res

  let psig tag = function
    | [] -> suffix ':' ~after:tag
    | si :: sg as items ->
      let sg = !pp_sig si sg in
      let res =
        match pp_token ~after:tag ~before:sg COLON with
        | colon -> tag ^^ nest 2 (colon ^/^ sg)
        | exception Assert_failure _ ->
          (* Can disappear when we properly handle exts and attrs on kw *)
          suffix ~after:tag ':' ^^ nest 2 (break_before ~spaces:1 sg)
      in
      if !sig_ends_in_obj items then break_after res else res

  let ptyp tag ct =
    let break_after =
      if !ct_ends_in_obj ct then break_after ~spaces:1 else (fun x -> x)
    in
    let ct = break_after (!pp_core_type ct) in
    let colon = pp_token ~after:tag ~before:ct COLON in
    tag ^^ nest 2 (colon ^/^ ct)

  let ppat tag p =
    let p = !pp_pattern p in
    let qmark = pp_token ~after:tag ~before:p QUESTION in
    tag ^^ nest 2 (qmark ^/^ p)

  let ppat_guard tag p e =
    let p = !pp_pattern p in
    let e = !pp_expression e in
    let qmark = pp_token ~after:tag ~before:p QUESTION in
    let when_ = pp_token ~after:p ~before:e WHEN in
    tag ^^ nest 2 (qmark ^/^ p ^/^ group (when_ ^/^ e))

  let pp_after ~tag = function
    | PStr st -> pstr tag st
    | PSig sg -> psig tag sg
    | PTyp ct -> ptyp tag ct
    | PPat (p, None) -> ppat tag p
    | PPat (p, Some e) -> ppat_guard tag p e
end


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
  let tag =
    let open Location in string ~loc:attr_name.loc (ats kind ^ attr_name.txt)
  in
  group (brackets (Payload.pp_after ~tag attr_payload))

(* :/ *)
let pp_doc ~loc = function
  | PStr
      [ { pstr_desc =
            Pstr_eval
              ({ pexp_desc = Pexp_constant Pconst_string (s, None); pexp_loc; _
              },
              []);
          _ } ]
    ->
    docstring ~loc s pexp_loc
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
  | _ -> pp_attr kind attr_name attr_payload

let attach ?(spaces=1) kind doc attrs =
  let postdoc, attrs =
    match
      List.partition (fun { attr_name; _ } -> attr_name.txt = "ocaml.doc") attrs
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
      group
        (prefix ~indent:2 ~spaces doc
          (separate_map (PPrint.break 0) ~f:(pp kind) attr attrs))
  in
  match postdoc with
  | None -> with_attrs
  | Some { attr_payload; attr_loc; _ } ->
    let doc =
      prefix ~indent:0 ~spaces with_attrs (pp_doc attr_payload ~loc:attr_loc)
    in
    if kind = Attached_to_structure_item then
      (* This is a ugly hack: we want a blank like to follow the document, so
         the post item docstring is not deemed ambiguous.
         However, if we add a hardline here, then there will be two blank
         lines: yirk!
         By adding enough space to go over 80c, we ensure that a blank line
         will follow. But also, we know that PPrint doesn't print trailing
         whitespaces. So the these, while being counted towards the document
         {!requirement} will never actually get printed. *)
      break_after ~spaces:81 doc
    else
      doc

let is_non_doc attr =
  match attr.attr_name.txt with
  | "ocaml.doc" | "ocaml.text" -> false
  | _ -> true

let has_non_doc =
  List.exists is_non_doc
let attach_to_item ?spaces doc =
  attach ?spaces Attached_to_item doc
let attach_to_top_item doc =
  attach Attached_to_structure_item doc

let extract_text ~item_start_pos =
  let rec aux acc = function
    | { attr_name = { txt = "ocaml.text" | "ocaml.doc"; _ }; attr_loc; _ }
        as attr ::
        attrs
      when
        Source_parsing.Comments.compare_pos attr_loc.loc_start item_start_pos <=
          0
      ->
      aux (attr :: acc) attrs
    | attrs -> List.rev acc, attrs
  in
  aux []

let prepend_text attrs doc =
  let text, docstring =
    List.partition (fun { attr_name; _ } -> attr_name.txt = "ocaml.text") attrs
  in
  let doc =
    match docstring with
    | [] -> doc
    | [ { attr_loc = loc; attr_payload; _ } ] ->
      concat ~sep:hardline (pp_doc ~loc attr_payload) doc
    | _ -> assert false
  in
  match text with
  | [] -> [ doc ]
  | text :: texts ->
    let texts =
      separate_map PPrint.hardline ~f:(fun { attr_payload; attr_loc; _ } ->
        pp_doc ~loc:attr_loc attr_payload
      ) text texts
    in
    [ texts; doc ]

module Extension = struct
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

