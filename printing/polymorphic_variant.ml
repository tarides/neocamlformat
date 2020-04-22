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
      let of_params =
        let of_ = token_between tag params Of in
        if has_empty_constr then
          let sep = PPrint.(break 1 ^^ ampersand ^^ break 1) in
          concat of_ ~sep params
        else
          of_ ^/^ params
      in
      tag ^/^ of_params

  let pp { prf_desc; prf_attributes; _ } =
    let desc = pp_desc prf_desc in
    !attach_attributes desc prf_attributes
end


(* Looks a lot like [left_assoc_map] … except for the first element...
   TODO: generalize a bit [left_assoc_map] and rename it to [dock] *)
let dock_fields ~opening_token x xs =
  let fmt x = nest 2 (group (break_before x)) in
  List.fold_left
    (fun acc elt ->
        let elt = fmt elt in
        let pipe = token_between acc elt Pipe in
        acc ^/^ group (pipe ^^ elt))
    (group (opening_token ^^ fmt x))
    xs

let pp_row_prefix ~loc ~opening_token x xs =
  let x = Row_field.pp x in
  let xs = List.map Row_field.pp xs in
  let opening_token = token_before ~start:loc.loc_start x opening_token in
  dock_fields ~opening_token x xs

let pp_simple_row ~loc ~opening_token ~hang_indent = function
  | [] -> 
    let lbracket = token_between_locs loc.loc_start loc.loc_end opening_token in
    let rbracket = token_after ~stop:loc.loc_end lbracket Rbracket in
    group (lbracket ^/^ rbracket)
  | x :: xs ->
    let fields = pp_row_prefix ~loc ~opening_token x xs in
    let rbracket = token_after ~stop:loc.loc_end fields Rbracket in
    hang hang_indent (fields ^/^ rbracket)

let pp_mixed_row ~loc ~labels:(l, ls) = function
  | [] -> assert false (* always at least one field *)
  | x :: xs ->
    let fields = pp_row_prefix ~loc ~opening_token:Open_variant x xs in
    let labels = flow_map (PPrint.break 1) Tag.pp l ls in
    let sep = token_between fields labels Rangle in
    let rbracket = token_after ~stop:loc.loc_end labels Rbracket in
    hang 1 (fields ^/^ sep ^/^ labels ^/^ rbracket)

let pp_row ~loc fields closed present =
  match closed, present with
  | Open, Some _ -> assert false
  | Closed, None ->
    pp_simple_row ~loc ~opening_token:Lbracket ~hang_indent:0 fields
  | Open, None -> 
    pp_simple_row ~loc ~opening_token:Open_variant ~hang_indent:1 fields
  | Closed, Some [] ->
    pp_simple_row ~loc ~opening_token:Closed_variant ~hang_indent:1 fields
  | Closed, Some (l::ls) ->
    pp_mixed_row ~loc ~labels:(l, ls) fields
