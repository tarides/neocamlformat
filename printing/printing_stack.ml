open Source_parsing
open Source_tree

type core_type_elt =
  | Arrow
  | Tuple

type elt =
  | Attribute
  | With_constraint
  | Core_type of core_type_desc
  | Pattern of pattern_desc
  | Expression of expression_desc
  | Class_expr of class_expr_desc
  | Module_type of module_type_desc
  | Function_parameter
  | Value_binding
  | Cons_constr of { on_left: bool }
  | Pipe of { on_left: bool }
  | Then_branch
  | Prefix_op
  | Infix_op of { on_left: bool; level: int (* gloups *); }
  | Row_field
  | Record_field
  | Unpack

let string_of_elt = function
  | Attribute -> "attr"
  | With_constraint -> "with constraint"
  | Core_type _ -> "core_type"
  | Pattern _ -> "patt"
  | Expression _ -> "expr"
  | Then_branch -> "if-then"
  | Class_expr _ -> "cl-expr"
  | Module_type _ -> "mty"
  | Function_parameter -> "param"
  | Value_binding -> "vb"
  | Cons_constr { on_left } -> if on_left then "_ ::" else ":: _"
  | Pipe { on_left } -> if on_left then "_ |" else "| _"
  | Prefix_op -> "pre-op"
  | Infix_op { on_left; level } ->
    Printf.sprintf "%s`%d`%s"
      (if on_left then "_ " else "")
      level
      (if on_left then "" else " _")
  | Row_field -> "row-field"
  | Record_field -> "rec-field"
  | Unpack -> "(val _)"

(* Cf.  http://caml.inria.fr/pub/docs/manual-ocaml/expr.html#ss:precedence-and-associativity *)
let infix_op ~on_left = function
  | "" -> assert false
  | "::" -> Cons_constr { on_left }
  | "|" -> Pipe { on_left }
  | "<-" | ":=" -> Infix_op { on_left; level = 1}
  | "or" | "||" -> Infix_op { on_left; level = 2}
  | "&"  | "&&" -> Infix_op { on_left; level = 3}
  | "!=" -> Infix_op { on_left; level = 4}
  | "mod" | "land" | "lor" | "lxor" -> Infix_op { on_left; level = 7}
  | "lsl" | "lsr" | "asr" -> Infix_op { on_left; level = 8}
  | s ->
    match String.get s 0 with
    | '=' | '<' | '>' | '|' | '&' | '$' -> Infix_op { on_left; level = 4}
    | '@' | '^' -> Infix_op { on_left; level = 5}
    | '+' | '-' -> Infix_op { on_left; level = 6}
    | '/' | '%' ->  Infix_op { on_left; level = 7}
    | '*' -> begin
        match String.get s 1 with
        | '*' -> Infix_op { on_left; level = 8}
        | _ | exception _ -> Infix_op { on_left; level = 7}
      end
    | '#' -> Infix_op { on_left; level = 9}
    | _ -> assert false

let top_is_op ~on_left op = function
  | [] -> [ infix_op ~on_left op ]
  | _ :: xs -> infix_op ~on_left op :: xs

type t = elt list

(* Refer to:
   - http://caml.inria.fr/pub/docs/manual-ocaml/types.html
   - http://caml.inria.fr/pub/docs/manual-ocaml/patterns.html
   - http://caml.inria.fr/pub/docs/manual-ocaml/expr.html#ss:precedence-and-associativity
   *)

let needs_parens elt parents =
  match elt with

  (* Type expressions *)

  (* N.B. arrows are right assoc, but since we've changed the parser and AST to
     make Ptyp_arrow n-ary, we don't need to care about that.
     If we have an arrow under another one, it means the user put parentheses,
     and the code here preserves them.  *)
  | Core_type Ptyp_arrow _
  | Core_type Ptyp_tuple _ ->
    List.exists (function
        | Core_type ( Ptyp_constr _
                    | Ptyp_class _
                    | Ptyp_arrow _
                    | Ptyp_tuple _) -> true
        | _ -> false) parents
  | Core_type Ptyp_alias _ ->
    List.exists (function
        | Core_type ( Ptyp_constr _
                    | Ptyp_class _
                    | Ptyp_arrow _
                    | Ptyp_tuple _) -> true
        | _ -> false) parents

  (* Patterns *)

  | Pattern Ppat_lazy _ ->
    List.exists (function
        | Function_parameter
        | Value_binding
        | Pattern Ppat_open _ -> true
        | Pattern (Ppat_construct _ | Ppat_variant _) ->
          (* Not necessary: but better style. *)
          true
        | Expression (Pexp_match _ | Pexp_try _)  ->
          (* we don't require parens at the top of a match. *)
          false
        | _ -> false) parents
  | Pattern Ppat_variant (_, Some _)
  | Pattern Ppat_construct (_, Some _) ->
    List.exists (function
        | Pattern (Ppat_lazy _ | Ppat_open _)
        | Function_parameter
        | Value_binding -> true
        | Pattern ( Ppat_construct _
                  | Ppat_variant _
                  | Ppat_exception _) ->
          (* Not necessary: but better style. *)
          true
        | _ -> false) parents
  | Cons_constr _ ->
    List.exists (function
        | Pattern ( Ppat_lazy _
                  | Ppat_construct _
                  | Ppat_variant _
                  | Ppat_open _)
        | Function_parameter
        | Value_binding ->
          true
        | Pattern Ppat_exception _ ->
          (* Not necessary: but better style. *)
          true
        | Prefix_op
        | Attribute
        | Class_expr Pcl_apply _
        | Expression ( Pexp_field _
                     | Pexp_setfield _
                     | Pexp_array_get _
                     | Pexp_array_set _
                     | Pexp_bigarray_get _
                     | Pexp_bigarray_set _
                     | Pexp_string_get _
                     | Pexp_string_set _
                     | Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     ) ->
          true
        | Infix_op { level; _ } -> level >= 6
        | Cons_constr { on_left } -> on_left
        | _ -> false) parents
  | Pattern Ppat_tuple _ ->
    List.exists (function
        | Pattern ( Ppat_tuple _
                  | Ppat_construct _
                  | Ppat_variant _
                  | Ppat_lazy _
                  | Ppat_exception _
                  | Ppat_open _)
        | Cons_constr _
        | Function_parameter
        | Value_binding -> true
        | _ -> false) parents
  | Pipe _
  | Pattern Ppat_or _ ->
    List.exists (function
        | Pattern Ppat_alias _ (* Not necessary: but better style. *)
        | Record_field (* Not necessary: but better style. *)
        | Pattern _
        | Attribute
        | Function_parameter
        | Value_binding
        | Cons_constr _ ->
          true
        | Pipe { on_left } -> not on_left
        | _ -> false) parents
  | Pattern Ppat_alias _ ->
    List.exists (function
        | Pattern _
        | Function_parameter
        | Value_binding
        | Cons_constr _ -> true
        | _ -> false) parents
  | Pattern Ppat_record _ ->
    List.exists (function
        | Function_parameter -> true
        | _ -> false) parents

  (* Expressions *)

  | Prefix_op ->
    List.exists (function
        | Prefix_op -> true
        | _ -> false) parents

  | Expression Pexp_field _
  | Expression Pexp_array_get _
  | Expression Pexp_bigarray_get _
  | Expression Pexp_string_get _ ->
    List.exists (function
        | Prefix_op -> true
        | _ -> false) parents

  | Expression Pexp_send _
  | Infix_op { level = 9; _ } ->
    (* #... : left-assoc *)
    List.exists (function
      | Prefix_op
      | Expression ( Pexp_field _
                    | Pexp_array_get _
                    | Pexp_bigarray_get _
                    | Pexp_string_get _)
      | Infix_op { level = 9; on_left = false } ->
        true
      | Expression Pexp_record _
      | Unpack ->
        (* Not described by the precedence table, but won't parse otherwise. *)
        true
      | _ -> false) parents

  | Expression Pexp_apply _
  | Expression Pexp_construct (_, Some _)
  | Expression Pexp_variant (_, Some _)
  | Expression Pexp_assert _
  | Expression Pexp_lazy _ ->
    List.exists (function
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _)
        | Infix_op { level = 9; _ }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     ) ->
          true
        | Expression Pexp_record _
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents

  | Attribute ->
    List.exists (function
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _)
        | Infix_op { level = 9; _ }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     ) ->
          true
        | With_constraint
        | Row_field
        | Record_field
        | Core_type (Ptyp_arrow _ | Ptyp_tuple _)
        | Cons_constr { on_left = false }
        | Expression Pexp_record _
        | Function_parameter
        | Value_binding
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents

  | Infix_op { level = 8; _ } ->
    (* **.. lsl lsr asr : right-assoc *)
    List.exists (function
      | Attribute
      | Prefix_op
      | Expression ( Pexp_field _
                    | Pexp_array_get _
                    | Pexp_bigarray_get _
                    | Pexp_string_get _)
      | Infix_op { level = 9; _ }
      | Infix_op { level = 8; on_left = true }
      | Class_expr Pcl_apply _
      | Expression ( Pexp_apply _
                    | Pexp_construct _
                    | Pexp_variant _
                    | Pexp_assert _
                    | Pexp_lazy _
                    ) ->
        true
      | Expression Pexp_record _
      | Unpack ->
        (* Not described by the precedence table, but won't parse otherwise. *)
        true
      | _ -> false) parents

  | Infix_op { level = 7; _ } ->
    (* *.. /.. %.. mod land lor lxor: left-assoc *)
    List.exists (function
        | Attribute
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _) -> true
        | Infix_op { level = (8 | 9); _ }
        | Infix_op { level = 7; on_left = false }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     ) ->
          true
        | Expression Pexp_record _
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents

  | Infix_op { level = 6; _ } ->
    (* +.. -..: left-assoc *)
    List.exists (function
        | Attribute
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _) -> true
        | Infix_op { level = (7 | 8 | 9); _ }
        | Infix_op { level = 6; on_left = false }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     ) ->
          true
        | Expression Pexp_record _
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents

  | Infix_op { level = 5; _ } ->
    (* @.. ^..: right-assoc *)
    List.exists (function
        | Attribute
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _) -> true
        | Infix_op { level = (6 | 7 | 8 | 9); _ }
        | Infix_op { level = 5; on_left = true }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     )
        | Cons_constr _ ->
          true
        | Expression Pexp_record _
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents

  | Infix_op { level = 4; _ } ->
    (* =.. <.. >.. |.. &.. $.. != : left-assoc *)
    List.exists (function
        | Attribute
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _) -> true
        | Infix_op { level = (5 | 6 | 7 | 8 | 9); _ }
        | Infix_op { level = 4; on_left = false }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     )
        | Cons_constr _ ->
          true
        | Expression Pexp_record _
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents


  | Infix_op { level = 3; _ } ->
    (* & && : right-assoc *)
    List.exists (function
        | Attribute
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _) -> true
        | Infix_op { level = (4 | 5 | 6 | 7 | 8 | 9); _ }
        | Infix_op { level = 3; on_left = true }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     )
        | Cons_constr _ ->
          true
        | Expression Pexp_record _
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents

  | Infix_op { level = 2; _ } ->
    (* or || : right-assoc *)
    List.exists (function
        | Attribute
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _) -> true
        | Infix_op { level = (3 | 4 | 5 | 6 | 7 | 8 | 9); _ }
        | Infix_op { level = 2; on_left = true }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     )
        | Cons_constr _ ->
          true
        | Expression Pexp_record _
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents

  | Expression Pexp_tuple _ ->
    (* <- := : right-assoc *)
    List.exists (function
        | Attribute
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _) -> true
        | Infix_op { level = (2 | 3 | 4 | 5 | 6 | 7 | 8 | 9); _ }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     | Pexp_tuple _
                     )
        | Cons_constr _ ->
          true
        | _ -> false) parents

  | Expression Pexp_setfield _
  | Expression Pexp_array_set _
  | Expression Pexp_string_set _
  | Expression Pexp_bigarray_set _
  | Infix_op { level = 1; _ } ->
    List.exists (function
        | Attribute
        | Prefix_op
        | Expression ( Pexp_field _
                     | Pexp_array_get _
                     | Pexp_bigarray_get _
                     | Pexp_string_get _) -> true
        | Infix_op { level = (2 | 3 | 4 | 5 | 6 | 7 | 8 | 9); _ }
        | Infix_op { level = 1; on_left = true }
        | Class_expr Pcl_apply _
        | Expression ( Pexp_apply _
                     | Pexp_construct _
                     | Pexp_variant _
                     | Pexp_assert _
                     | Pexp_lazy _
                     | Pexp_tuple _
                     )
        | Cons_constr _ ->
          true
        | Expression Pexp_record _
        | Unpack ->
          (* Not described by the precedence table, but won't parse otherwise. *)
          true
        | _ -> false) parents

  | Expression Pexp_ifthenelse _ ->
    List.exists (function
      | Attribute
      | Prefix_op
      | Infix_op _
      | Class_expr Pcl_apply _
      | Expression ( Pexp_apply _
                   | Pexp_assert _
                   | Pexp_lazy _
                   | Pexp_construct _
                   | Pexp_variant _
                   | Pexp_record _
                   | Pexp_array _
                   | Pexp_tuple _
                   | Pexp_sequence _
                   | Pexp_send _
                   )
      | Then_branch
      | Record_field
      | Unpack -> true
      | _ -> false) parents

  | Expression Pexp_object _
  | Expression Pexp_for _
  | Expression Pexp_while _ ->
    List.exists (function
      | Prefix_op
      | Infix_op _
      | Class_expr Pcl_apply _
      | Expression ( Pexp_apply _
                    | Pexp_assert _
                    | Pexp_lazy _
                    | Pexp_construct _
                    | Pexp_variant _
                    | Pexp_send _
                    )
      | Record_field -> true
      | _ -> false) parents

  | Expression Pexp_sequence _ ->
    List.exists (function
      | Attribute
      | Prefix_op
      | Infix_op _
      | Class_expr Pcl_apply _
      | Expression ( Pexp_apply _
                    | Pexp_assert _
                    | Pexp_lazy _
                    | Pexp_construct _
                    | Pexp_variant _
                    | Pexp_record _
                    | Pexp_array _
                    | Pexp_ifthenelse _
                    | Pexp_list_lit _
                    | Pexp_sequence _
                    | Pexp_send _
                    | Pexp_tuple _
                    )
      | Then_branch
      | Record_field
      | Unpack -> true
      | _ -> false) parents

  | Class_expr Pcl_fun _
  | Class_expr Pcl_let _
  | Class_expr Pcl_open _ ->
    List.exists (function
      | Attribute
      | Class_expr Pcl_apply _ -> true
      | _ -> false) parents

  | Expression Pexp_letopen _
  | Expression Pexp_let _ ->
    List.exists (function
      | Attribute
      | Class_expr Pcl_apply _
      | Expression ( Pexp_apply _
                    | Pexp_construct _
                    | Pexp_variant _
                    | Pexp_assert _
                    | Pexp_lazy _
                    | Pexp_record _
                    | Pexp_tuple _
                    | Pexp_field _
                    | Pexp_array_get _
                    | Pexp_bigarray_get _
                    | Pexp_string_get _
                    | Pexp_setfield _
                    | Pexp_sequence _) -> true
      | Prefix_op
      | Infix_op { on_left = true; _ }
      | Cons_constr { on_left = true }
      | Record_field
      | Unpack -> true
      | _ -> false) parents
  | Expression Pexp_fun _
  | Expression Pexp_function _ ->
    List.exists (function
      | Value_binding
      | Expression Pexp_fun _
      | Expression Pexp_function _
      | Infix_op { on_left = false; _ }->
        false
      | _ -> true) parents
  | Expression Pexp_match _
  | Expression Pexp_try _ ->
    List.exists (function
      | Attribute
      | Class_expr Pcl_apply _
      | Expression ( Pexp_apply _
                    | Pexp_construct _
                    | Pexp_variant _
                    | Pexp_assert _
                    | Pexp_lazy _
                    | Pexp_match _
                    | Pexp_try _
                    | Pexp_function _
                    | Pexp_list_lit _
                    | Pexp_sequence _
                    | Pexp_record _
                    | Pexp_tuple _)
      | Prefix_op
      | Infix_op { on_left = true; _ }
      | Cons_constr { on_left = true }
      | Record_field
      | Unpack -> true
      | _ -> false) parents

  | Module_type (Pmty_typeof _) ->
    List.exists (function
      | Attribute -> true
      | _ -> false) parents

  | _ -> false

(* FIXME *)
let rec normalize = function
  | ([] | [ _ ]) as lst -> lst

  | Expression
      ( Pexp_fun _
      | Pexp_function _
      | Pexp_let _
      | Pexp_letexception _
      | Pexp_letmodule _
      | Pexp_letop _
      | Pexp_match _
      | Pexp_open _
      | Pexp_try _ ) as keep ::
    ( Then_branch | Expression Pexp_ifthenelse _
    | Infix_op { on_left = false; _ }) :: tl
    -> normalize (keep :: tl)

  | x :: xs -> x :: normalize xs

let parenthesize ?(situations=Options.Situations.When_needed)
    ?(style=Options.Parenthesing.Parens) t =
  let enclosed doc =
    let open Document in
    match style with
    | Parens ->
      let indented = nest 1 doc in
      parens indented
    | Begin_end ->
      let indented =
        match List.hd t with
        | Expression (Pexp_match _ | Pexp_try _) -> doc
        | _ -> nest 2 doc
      in
      enclose ~before:PPrint.(!^"begin ") ~after:PPrint.(hardline ^^ !^"end")
        indented
  in
  match situations with
  | Always ->
    let t =
      match t with
      | [] -> []
      | top :: _ -> [ top ]
    in
    t, enclosed
  | When_needed ->
    match t with
    | [] -> assert false
    | elt :: parents ->
      let parents = normalize parents in
      if needs_parens elt parents then
        [ elt ], enclosed
      else
        elt :: parents, Fun.id
