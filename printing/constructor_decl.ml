open Source_parsing
open Source_tree
open Document

(******************************************************)
(* Waiting for: https://github.com/ocaml/RFCs/pull/11 *)
(******************************************************)

let pp_record : (loc:Location.t -> label_declaration list -> t) ref =
  ref (fun ~loc:_ _ -> assert false)
let pp_core_type : (core_type -> t) ref =
  ref (fun _ -> assert false)
let attach_attributes : (t -> attributes -> t) ref =
  ref (fun _ _ -> assert false)
let pp_longident : (Longident.t -> t) ref =
  ref (fun _ -> assert false)

(******************************************************)

let has_args = function
  | Pcstr_tuple [] -> false
  | _ -> true

let constructor_arguments = function
  | Pcstr_tuple [] -> assert false (* [has_args] was called *)
  | Pcstr_record (loc, lbl_decls) -> !pp_record ~loc lbl_decls
  | Pcstr_tuple (a1 :: args) ->
    left_assoc_map ~sep:STAR ~f:!pp_core_type a1 args

let constructor_name name =
  match Ident_class.classify name with
  | Normal -> str name
  | _ -> Document.parens (str name)

let gadt_constructor name args res_ty attributes =
  let name = constructor_name name in
  let decl =
    if has_args args then
      let args = constructor_arguments args in
      let res  = !pp_core_type (Option.get res_ty) in
      let colon = pp_token ~after:name ~before:args COLON in
      let arrow = pp_token ~after:args ~before:res MINUSGREATER in
      group (
        name ^^
        nest 2 (
          break_before (group (colon ^^ nest 2 (break_before args)))
          ^/^ group (arrow ^^ nest 2 (break_before res))
        )
      )
    else
      let res  = !pp_core_type (Option.get res_ty) in
      Two_separated_parts.sep_with_second name res ~sep:COLON
  in
  !attach_attributes decl attributes

let simple_constructor name args attributes =
  let name = constructor_name name in
  let decl =
    if has_args args then
      let args = constructor_arguments args in
      Two_separated_parts.sep_with_first name args ~sep:OF
    else
      name
  in
  !attach_attributes decl attributes

let pp_constructor name args res_ty attributes =
  match res_ty with
  | None -> simple_constructor name args attributes
  | Some _ -> gadt_constructor name args res_ty attributes

let pp_rebind name rebound attributes =
  let name = str name in
  let rebound = !pp_longident rebound in
  let decl = Two_separated_parts.sep_with_first name rebound ~sep:EQUAL in
  !attach_attributes decl attributes

let pp_decl { pcd_name; pcd_args; pcd_res; pcd_attributes; _ } =
  pp_constructor pcd_name pcd_args pcd_res pcd_attributes

let pp_extension { pext_name; pext_kind; pext_attributes; _ } =
  match pext_kind with
  | Pext_decl (args, res_ty) ->
    pp_constructor pext_name args res_ty pext_attributes
  | Pext_rebind lid ->
    pp_rebind pext_name lid pext_attributes
