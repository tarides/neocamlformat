open Document
open Custom_combinators
open Import
open Source_tree

(******************************************************)
(* Waiting for: https://github.com/ocaml/RFCs/pull/11 *)
(******************************************************)

let pp_record : (loc:Location.t -> label_declaration list -> t) ref =
  ref (fun ~loc:_ _ -> assert false)

(******************************************************)

let has_args = function
  | Pcstr_tuple [] -> false
  | _ -> true

let constructor_arguments = function
  | Pcstr_tuple [] -> assert false (* [has_args] was called *)
  | Pcstr_record (loc, lbl_decls) -> !pp_record ~loc lbl_decls
  | Pcstr_tuple (a1 :: args) -> left_assoc_map ~sep:STAR ~f:Core_type.pp a1 args

let constructor_name name =
  (* FIXME: remove if there's no issue with (::) in tests *)
  (*
  match Ident_class.classify name with
  | Normal -> str name
  | _ -> Document.parens (str name)
  *)
  str name

let gadt_constructor ~is_exn name vars args res_ty attributes =
  let name = constructor_name name in
  let decl =
    if has_args args then
      let args = constructor_arguments args in
      let args =
        match vars with
        | [] -> args
        | v :: vs ->
          let vars = separate_map (break 1) ~f:str v vs in
          Two_separated_parts.sep_with_first vars args ~sep:DOT
      in
      let res = Core_type.pp (Option.get res_ty) in
      let colon = Token.pp ~after:name ~before:args COLON in
      let arrow = Token.pp ~after:args ~before:res MINUSGREATER in
      prefix ~indent:2 ~spaces:1 name
        (prefix ~indent:2 ~spaces:1 colon args ^/^
         prefix ~indent:2 ~spaces:1 arrow res)
    else
      let res = Core_type.pp (Option.get res_ty) in
      let res =
        match vars with
        | [] -> res
        | v :: vs ->
          let vars = separate_map (break 1) ~f:str v vs in
          Two_separated_parts.sep_with_first vars res ~sep:DOT
      in
      Two_separated_parts.sep_with_second name res ~sep:COLON
  in
  let kind : Attribute.kind =
    if is_exn
    then Attached_to_exception
    else Attached_to_item
  in
  Attribute.attach kind decl attributes

let simple_constructor ~is_exn name args attributes =
  let name = constructor_name name in
  let decl =
    if has_args args then
      let args = constructor_arguments args in
      Two_separated_parts.sep_with_first name args ~sep:OF
    else
      name
  in
  let kind : Attribute.kind =
    if is_exn
    then Attached_to_exception
    else Attached_to_item
  in
  Attribute.attach kind decl attributes

let pp_constructor ~is_exn name vars args res_ty attributes =
  match res_ty with
  | None -> simple_constructor ~is_exn name args attributes
  | Some _ -> gadt_constructor ~is_exn name vars args res_ty attributes

let pp_rebind name rebound attributes =
  let name = str name in
  let rebound = Longident.pp rebound in
  let decl = Two_separated_parts.sep_with_first name rebound ~sep:EQUAL in
  Attribute.attach Attached_to_exception decl attributes

let pp_decl { pcd_name; pcd_vars; pcd_args; pcd_res; pcd_attributes; _ } =
  pp_constructor ~is_exn:false pcd_name pcd_vars pcd_args pcd_res pcd_attributes

let pp_extension ?(is_exn=false) { pext_name; pext_kind; pext_attributes; _ } =
  match pext_kind with
  | Pext_decl (vars, args, res_ty) ->
    pp_constructor ~is_exn pext_name vars args res_ty pext_attributes
  | Pext_rebind lid -> pp_rebind pext_name lid pext_attributes
