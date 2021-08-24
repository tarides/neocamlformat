open Document
open Source_parsing
open Asttypes
open Source_tree
open Location

let pp_core_type : (Printing_stack.t -> core_type -> Document.t) ref =
  ref (fun _ _ -> assert false)

let attach_attributes : (Document.t -> attributes -> Document.t) ref =
  ref (fun _ _ -> assert false)

module Tag = struct
  let pp tag = string ~loc:tag.loc ("`" ^ tag.txt)
end

module Row_field : sig
  val pp : row_field -> Document.t
end = struct
  let pp_params p ps =
    let sep = PPrint.(break 1 ^^ ampersand ^^ break 1) in
    separate_map sep ~f:(!pp_core_type [ Row_field ]) p ps

  let pp_desc = function
    | Rinherit ct -> !pp_core_type [] ct
    | Rtag (tag, _, []) ->
      Tag.pp tag
    | Rtag (tag, has_empty_constr, p :: ps) ->
      let tag = Tag.pp tag in
      let params = pp_params p ps in
      let params =
        if not has_empty_constr then
          params
        else
          PPrint.ampersand ++ break_before params
      in
      Two_separated_parts.sep_with_first tag params ~sep:OF

  let pp { prf_desc; prf_attributes; _ } =
    let desc = pp_desc prf_desc in
    !attach_attributes desc prf_attributes
end


(* Looks a lot like [left_assoc_map] … except for the first element...
   TODO: generalize a bit [left_assoc_map] and rename it to [dock] *)
let dock_fields ~opening_token x xs =
  let fmt x = nest 2 (PPrint.space ++ x) in
  List.fold_left
    (fun acc elt ->
        let elt = fmt elt in
        let pipe = token_between acc elt BAR in
        acc ^/^ group (pipe ^^ elt))
    (group (opening_token ^^ fmt x))
    xs

let pp_row_prefix ~prefix_if_necessary ~loc ~opening_token x xs =
  let needs_prefix =
    prefix_if_necessary && opening_token = Source_parsing.Parser.LBRACKET &&
    xs = [] && (match x.prf_desc with Rinherit _ -> true | _ -> false)
  in
  let x = Row_field.pp x in
  let xs = List.map Row_field.pp xs in
  let opening_token = token_before ~start:loc.loc_start x opening_token in
  let opening_token =
    if not needs_prefix then
      opening_token
    else
      let pipe = token_between opening_token x BAR in
      opening_token ^/^ pipe
  in
  dock_fields ~opening_token x xs

let pp_simple_row ~loc ~opening_token ~hang_indent = function
  | [] -> 
    let lbracket = token_between_locs loc.loc_start loc.loc_end opening_token in
    let rbracket = token_after ~stop:loc.loc_end lbracket RBRACKET in
    group (lbracket ^/^ rbracket)
  | x :: xs ->
    let fields =
      pp_row_prefix ~prefix_if_necessary:true ~loc ~opening_token x xs
    in
    let rbracket = token_after ~stop:loc.loc_end fields RBRACKET in
    hang hang_indent (fields ^/^ rbracket)

let pp_mixed_row ~loc ~labels:(l, ls) = function
  | [] -> assert false (* always at least one field *)
  | x :: xs ->
    let fields =
      pp_row_prefix ~prefix_if_necessary:false ~loc
        ~opening_token:LBRACKETLESS x xs
    in
    let labels = flow_map (PPrint.break 1) Tag.pp l ls in
    let sep = token_between fields labels GREATER in
    let rbracket = token_after ~stop:loc.loc_end labels RBRACKET in
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
  | Closed, Some (l::ls) ->
    pp_mixed_row ~loc ~labels:(l, ls) fields
