open Source_tree

type core_type_elt =
  | Arrow
  | Tuple

type elt =
  | Core_type of core_type_desc
  | Pattern of pattern_desc
  | Expression of expression_desc
  | Value_binding (* FIXME: currently also used for function parameters *)
  | Row_field
  | Record_field
  | Unpack

type t = elt list

(* Refer to:
   http://caml.inria.fr/pub/docs/manual-ocaml/expr.html#ss:precedence-and-associativity
   *)

let needs_parens elt parents =
  match elt with
  | Core_type Ptyp_arrow _
  | Core_type Ptyp_tuple _ ->
    List.exists (function
      | Core_type (Ptyp_alias _ | Ptyp_arrow _ | Ptyp_tuple _) -> true
      | _ -> false
    ) parents
  | Core_type Ptyp_alias _ ->
    List.exists (function
      | Core_type (Ptyp_arrow _ | Ptyp_tuple _)
      | Value_binding -> true
      | _ -> false
    ) parents
  | Pattern Ppat_tuple _ ->
    List.exists (function
      | Pattern ( Ppat_tuple _
                | Ppat_construct _
                | Ppat_variant _
                | Ppat_exception _)
      | Value_binding -> true
      | _ -> false
    ) parents
  | Pattern Ppat_variant (_, Some _)
  | Pattern Ppat_construct (_, Some _) ->
    List.exists (function
      | Pattern ( Ppat_construct _
                | Ppat_variant _
                | Ppat_exception _) (* not strictly necessary *)
      | Value_binding -> true
      | _ -> false
    ) parents
  | Pattern Ppat_or _ ->
    List.exists (function
      | Pattern _ 
      | Value_binding -> true
      | Expression (Pexp_match _ | Pexp_try _)  ->
        (* we don't require parens at the top of a match. *)
        false
      | _ -> false
    ) parents
  | Pattern Ppat_lazy _ ->
    List.exists (function
      | Pattern (Ppat_construct _ | Ppat_variant _)
      | Value_binding -> true
      | Expression (Pexp_match _ | Pexp_try _)  ->
        (* we don't require parens at the top of a match. *)
        false
      | _ -> false
    ) parents
  | Expression Pexp_letopen _
  | Expression Pexp_let _ -> begin
      match parents with
      | Expression (Pexp_apply _ | Pexp_assert _) :: _ -> true
      | Unpack :: _ -> true
      | _ -> false
    end
  | Expression Pexp_fun _
  | Expression Pexp_function _ -> begin
      match parents with
      | Value_binding :: _ -> false
      | _ -> true
    end
  | Expression Pexp_assert _
  | Expression Pexp_apply _ -> begin
      match parents with
      | Expression ( Pexp_apply _
                   | Pexp_assert _
                   | Pexp_record _
                   | Pexp_field _
                   | Pexp_setfield _
                   | Pexp_send _) :: _
      | Unpack :: _ -> true
      | _ -> false
    end
  | Expression Pexp_match _
  | Expression Pexp_try _ ->
    List.exists (function
      | Expression ( Pexp_apply _
                   | Pexp_assert _
                   | Pexp_match _
                   | Pexp_try _
                   | Pexp_function _
                   | Pexp_sequence _)
      | Unpack -> true
      | _ -> false
    ) parents
  | Expression Pexp_tuple _ ->
    true (* FIXME *)
  | Expression Pexp_construct (_, Some _)
  | Expression Pexp_variant (_, Some _) ->
    List.exists (function
      | Expression ( Pexp_apply _
                   | Pexp_assert _
                   | Pexp_construct _
                   | Pexp_variant _)
      | Unpack -> true
      | _ -> false
    ) parents
  | Expression Pexp_sequence _ ->
    List.exists (function
      | Expression ( Pexp_apply _
                   | Pexp_assert _
                   | Pexp_construct _
                   | Pexp_variant _
                   | Pexp_record _
                   | Pexp_array _
                   | Pexp_ifthenelse _
                   (* not doing these would be semantically equivalent, but
                      would change the AST. *)
                   | Pexp_sequence _ 
                   | Pexp_send _
                   )
      | Record_field
      | Unpack -> true
      | _ -> false
    ) parents
  | Expression Pexp_send _ ->
    List.exists (function
      | Expression ( Pexp_apply _
                   | Pexp_assert _
                   | Pexp_construct _
                   | Pexp_variant _
                   | Pexp_record _) 
      | Unpack -> true
      | _ -> false
    ) parents
  | _ -> false

let parenthesize t doc =
  match t with
  | [] -> assert false
  | x :: xs when needs_parens x xs -> PPrint.parens doc
  | _ -> doc

let will_parenthesize t =
  match t with
  | [] -> assert false
  | x :: xs -> needs_parens x xs
