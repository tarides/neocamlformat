open Source_parsing
open Source_tree
open Location
open PPrint
open Custom_combinators

(******************************************************)
(* Waiting for: https://github.com/ocaml/RFCs/pull/11 *)
(******************************************************)

let pp_record : (label_declaration list -> document) ref =
  ref (fun _ -> assert false)
let pp_core_type : (Printing_stack.t -> core_type -> document) ref =
  ref (fun _ _ -> assert false)
let attach_attributes : (document -> attributes -> document) ref =
  ref (fun _ _ -> assert false)
let pp_longident : (Longident.t -> document) ref =
  ref (fun _ -> assert false)

(******************************************************)

let has_args = function
  | Pcstr_tuple [] -> false
  | _ -> true

let constructor_arguments = function
  | Pcstr_record lbl_decls -> !pp_record lbl_decls
  | Pcstr_tuple args ->
    let printing_stack =
      (* morally equivalent to: *)
      [ Printing_stack.Core_type (Ptyp_tuple args) ]
    in
    separate_map (break 1 ^^ star ^^ break 1)
      (!pp_core_type printing_stack) args

let gadt_constructor name args res_ty attributes =
  let name = string name.txt in
  let decl =
    if has_args args then
      let args = constructor_arguments args in
      let res  = !pp_core_type [] (Option.get res_ty) in
      name ^/^ colon ^/^ args ^/^ !^"->" ^/^ res 
    else
      let res  = !pp_core_type [] (Option.get res_ty) in
      name ^/^ colon ^/^ res 
  in
  !attach_attributes decl attributes

let simple_constructor name args attributes =
  let name = string name.txt in
  let decl =
    if has_args args then
      let args = constructor_arguments args in
      group (
        prefix ~indent:2 ~spaces:1
          (name ^/^ !^"of")
          args
      )
    else
      name
  in
  !attach_attributes decl attributes

let pp_constructor name args res_ty attributes =
  match res_ty with
  | None -> simple_constructor name args attributes
  | Some _ -> gadt_constructor name args res_ty attributes

let pp_rebind name rebound attributes =
  let name = string name.txt in
  let decl =
    group (
      group (name ^^ nest 2 (break 1 ^^ equals))
      ^^ nest 2 (break 1 ^^ !pp_longident rebound.txt)
    )
  in
  !attach_attributes decl attributes

let pp_decl { pcd_name; pcd_args; pcd_res; pcd_attributes; _ } =
  pp_constructor pcd_name pcd_args pcd_res pcd_attributes

let pp_extension { pext_name; pext_kind; pext_attributes; _ } =
  match pext_kind with
  | Pext_decl (args, res_ty) ->
    pp_constructor pext_name args res_ty pext_attributes
  | Pext_rebind lid ->
    pp_rebind pext_name lid pext_attributes
