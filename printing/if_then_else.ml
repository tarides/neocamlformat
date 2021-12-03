open Source_parsing
open Asttypes
open Source_tree
open Document

let imported_pp_exp : (expression -> t) ref = ref (fun _ -> assert false)
let imported_decorate :
  (t -> extension:string loc option -> attributes -> later:t -> t) ref =
  ref (fun _ ~extension:_ _ ~later:_ -> assert false)

let[@inline] pp exp = !imported_pp_exp exp

module Keyword : sig
  val decorate :
    t -> extension:string loc option -> attributes -> later:t -> t
end = struct

  let[@inline] decorate t = !imported_decorate t
end

type branch =
  | Expr of t
  | Delimited of { opening: t; expr: t; closing: t }

let fmt_branch = function
  | { pexp_desc = Pexp_parens { begin_end; exp };
      pexp_ext_attributes = (None, []);
      pexp_loc = loc; _ } as e ->
    begin match exp.pexp_desc with
    | Pexp_tuple _ -> (* dubious *) Expr (pp e)
    | _ ->
      let expr = pp exp in
      let opening =
        pp_token ~inside:loc ~before:expr (if begin_end then BEGIN else LPAREN)
      in
      let closing =
        pp_token ~inside:loc ~after:expr (if begin_end then END else RPAREN)
      in
      Delimited {opening; expr; closing}
    end
  | e -> Expr (pp e)

type previous_chunk = [
  | `None
  | `Terminated
  | `Requires_closing of t
]

let fmt_if_chunk ~(previous_chunk:previous_chunk) ib =
  let cond = pp ib.if_cond in
  let keyword =
    let if_kw =
      let if_ = pp_token ~inside:ib.if_loc ~before:cond IF in
      match previous_chunk with
      | `None -> if_
      | `Terminated | `Requires_closing _ as otherwise ->
        let else_ = pp_token ~inside:ib.if_loc ~before:if_ ELSE in
        let else_if = else_ ^/^ if_ in
        match otherwise with
        | `Terminated -> else_if
        | `Requires_closing closing -> group (closing ^/^ else_if)
    in
    Keyword.decorate if_kw ~extension:ib.if_ext ib.if_attrs ~later:cond
  in
  match fmt_branch ib.if_body with
  | Expr then_branch ->
    let then_kw = pp_token ~after:cond ~before:then_branch THEN in
    let if_and_cond =
      group (
        keyword ^^
        nest 2 (break_before cond) ^/^
        then_kw
      )
    in
    concat ~indent:2 ~sep:(break 1) if_and_cond then_branch, `Terminated
  | Delimited {opening; expr = then_branch; closing} ->
    let then_kw = pp_token ~after:cond ~before:then_branch THEN in
    let if_and_cond =
      group (
        keyword ^^
        nest 2 (break_before cond) ^/^
        group (then_kw ^/^ opening)
      )
    in
    concat ~indent:2 ~sep:(break 0) if_and_cond then_branch,
    `Requires_closing closing

let rec iterate_branches ?(previous_chunk=`None) = function
  | [] -> assert false
  | [ x ] -> fmt_if_chunk ~previous_chunk x
  | x :: xs ->
    let branch, chunk_type = fmt_if_chunk ~previous_chunk x in
    let other_branches, last_chunk_type =
      iterate_branches ~previous_chunk:chunk_type xs
    in
    branch ^/^ other_branches, last_chunk_type

let knr_if_then if_branches =
  let branches, last_chunk_type = iterate_branches if_branches in
  match last_chunk_type with
  | `Terminated -> branches
  | `Requires_closing t -> concat ~sep:(break 0) branches t

let knr_if_then_else if_branches else_branch =
  let ifs, last_chunk = iterate_branches if_branches in
  let else_ =
    let mk_else before = 
      match last_chunk with
      | `Terminated -> pp_token ~after:ifs ~before ELSE
      | `Requires_closing closing ->
        let else_ = pp_token ~after:closing ~before ELSE in
        group (closing ^/^ else_)
    in
    match fmt_branch else_branch with
    | Expr else_branch ->
      mk_else else_branch ^^
      nest 2 (break_before else_branch)
    | Delimited {opening; expr = else_branch; closing} ->
      group (mk_else opening ^/^ opening) ^^
      nest 2 (break_before else_branch) ^/^ closing
  in
  group (ifs ^^ group (break_before else_))
