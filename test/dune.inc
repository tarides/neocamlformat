
(rule
 (alias runtest)
 (target align_cases.ml.corrected)
 (deps (:t align_cases.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target align_infix.ml.corrected)
 (deps (:t align_infix.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target apply.ml.corrected)
 (deps (:t apply.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target array.ml.corrected)
 (deps (:t array.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target assignment_operator.ml.corrected)
 (deps (:t assignment_operator.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target attribute_and_expression.ml.corrected)
 (deps (:t attribute_and_expression.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target attributes.ml.corrected)
 (deps (:t attributes.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target break_before_in.ml.corrected)
 (deps (:t break_before_in.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target break_cases.ml.corrected)
 (deps (:t break_cases.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target break_fun_decl.ml.corrected)
 (deps (:t break_fun_decl.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target break_record.ml.corrected)
 (deps (:t break_record.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target break_separators.ml.corrected)
 (deps (:t break_separators.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target break_sequence_before.ml.corrected)
 (deps (:t break_sequence_before.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target break_string_literals.ml.corrected)
 (deps (:t break_string_literals.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target break_struct.ml.corrected)
 (deps (:t break_struct.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target cinaps.ml.corrected)
 (deps (:t cinaps.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target cmdline_override.ml.corrected)
 (deps (:t cmdline_override.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target cmdline_override2.ml.corrected)
 (deps (:t cmdline_override2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comment_breaking.ml.corrected)
 (deps (:t comment_breaking.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comment_header.ml.corrected)
 (deps (:t comment_header.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comment_in_empty.ml.corrected)
 (deps (:t comment_in_empty.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comment_in_modules.ml.corrected)
 (deps (:t comment_in_modules.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comment_last.ml.corrected)
 (deps (:t comment_last.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comment_sparse.ml.corrected)
 (deps (:t comment_sparse.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comments.ml.corrected)
 (deps (:t comments.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comments_args.ml.corrected)
 (deps (:t comments_args.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comments_around_disabled.ml.corrected)
 (deps (:t comments_around_disabled.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target comments_in_record.ml.corrected)
 (deps (:t comments_in_record.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target compact_lists_arrays.ml.corrected)
 (deps (:t compact_lists_arrays.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target custom_list.ml.corrected)
 (deps (:t custom_list.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target disabled.ml.corrected)
 (deps (:t disabled.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target disambiguate.ml.corrected)
 (deps (:t disambiguate.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target doc_comments.ml.corrected)
 (deps (:t doc_comments.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target doc_comments_padding.ml.corrected)
 (deps (:t doc_comments_padding.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target empty.ml.corrected)
 (deps (:t empty.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target empty_ml.ml.corrected)
 (deps (:t empty_ml.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target error1.ml.corrected)
 (deps (:t error1.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target error2.ml.corrected)
 (deps (:t error2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target error3.ml.corrected)
 (deps (:t error3.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target error4.ml.corrected)
 (deps (:t error4.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target exceptions.ml.corrected)
 (deps (:t exceptions.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target exp_grouping.ml.corrected)
 (deps (:t exp_grouping.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target exp_record.ml.corrected)
 (deps (:t exp_record.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target expect_test.ml.corrected)
 (deps (:t expect_test.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target extensions.ml.corrected)
 (deps (:t extensions.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target field.ml.corrected)
 (deps (:t field.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target first_class_module.ml.corrected)
 (deps (:t first_class_module.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target floating_doc.ml.corrected)
 (deps (:t floating_doc.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target for_while.ml.corrected)
 (deps (:t for_while.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target format_invalid_files.ml.corrected)
 (deps (:t format_invalid_files.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target fun_decl.ml.corrected)
 (deps (:t fun_decl.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target function_indent.ml.corrected)
 (deps (:t function_indent.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target functor.ml.corrected)
 (deps (:t functor.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target funsig.ml.corrected)
 (deps (:t funsig.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target gadt.ml.corrected)
 (deps (:t gadt.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target generative.ml.corrected)
 (deps (:t generative.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target index_op.ml.corrected)
 (deps (:t index_op.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target infix_arg_grouping.ml.corrected)
 (deps (:t infix_arg_grouping.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target infix_bind.ml.corrected)
 (deps (:t infix_bind.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target infix_precedence.ml.corrected)
 (deps (:t infix_precedence.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target invalid.ml.corrected)
 (deps (:t invalid.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target invalid_docstring.ml.corrected)
 (deps (:t invalid_docstring.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue114.ml.corrected)
 (deps (:t issue114.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue289.ml.corrected)
 (deps (:t issue289.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue48.ml.corrected)
 (deps (:t issue48.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue51.ml.corrected)
 (deps (:t issue51.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue57.ml.corrected)
 (deps (:t issue57.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue60.ml.corrected)
 (deps (:t issue60.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue77.ml.corrected)
 (deps (:t issue77.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue85.ml.corrected)
 (deps (:t issue85.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target issue89.ml.corrected)
 (deps (:t issue89.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target ite.ml.corrected)
 (deps (:t ite.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target js_source.ml.corrected)
 (deps (:t js_source.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target kw_extentions.ml.corrected)
 (deps (:t kw_extentions.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target label_option_default_args.ml.corrected)
 (deps (:t label_option_default_args.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target lazy.ml.corrected)
 (deps (:t lazy.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target let_binding.ml.corrected)
 (deps (:t let_binding.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target let_in_constr.ml.corrected)
 (deps (:t let_in_constr.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target let_module.ml.corrected)
 (deps (:t let_module.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target list.ml.corrected)
 (deps (:t list.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target loc_stack.ml.corrected)
 (deps (:t loc_stack.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target locally_abtract_types.ml.corrected)
 (deps (:t locally_abtract_types.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target margin_80.ml.corrected)
 (deps (:t margin_80.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target match.ml.corrected)
 (deps (:t match.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target match2.ml.corrected)
 (deps (:t match2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target match_indent.ml.corrected)
 (deps (:t match_indent.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target max_indent.ml.corrected)
 (deps (:t max_indent.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target module.ml.corrected)
 (deps (:t module.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target module_anonymous.ml.corrected)
 (deps (:t module_anonymous.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target module_attributes.ml.corrected)
 (deps (:t module_attributes.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target module_item_spacing.ml.corrected)
 (deps (:t module_item_spacing.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target module_type.ml.corrected)
 (deps (:t module_type.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target monadic_binding.ml.corrected)
 (deps (:t monadic_binding.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target multi_index_op.ml.corrected)
 (deps (:t multi_index_op.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target need_format.ml.corrected)
 (deps (:t need_format.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target new.ml.corrected)
 (deps (:t new.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target object.ml.corrected)
 (deps (:t object.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target object_type.ml.corrected)
 (deps (:t object_type.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target ocp_indent_compat.ml.corrected)
 (deps (:t ocp_indent_compat.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target ocp_indent_options.ml.corrected)
 (deps (:t ocp_indent_options.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target open.ml.corrected)
 (deps (:t open.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target open_types.ml.corrected)
 (deps (:t open_types.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target option.ml.corrected)
 (deps (:t option.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target parens_tuple_patterns.ml.corrected)
 (deps (:t parens_tuple_patterns.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target precedence.ml.corrected)
 (deps (:t precedence.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target prefix_infix.ml.corrected)
 (deps (:t prefix_infix.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target print_config.ml.corrected)
 (deps (:t print_config.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target profiles.ml.corrected)
 (deps (:t profiles.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target profiles2.ml.corrected)
 (deps (:t profiles2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target protected_object_types.ml.corrected)
 (deps (:t protected_object_types.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target record.ml.corrected)
 (deps (:t record.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target record_punning.ml.corrected)
 (deps (:t record_punning.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target reformat_string.ml.corrected)
 (deps (:t reformat_string.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target refs.ml.corrected)
 (deps (:t refs.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target remove_extra_parens.ml.corrected)
 (deps (:t remove_extra_parens.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target revapply_ext.ml.corrected)
 (deps (:t revapply_ext.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target send.ml.corrected)
 (deps (:t send.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target sequence.ml.corrected)
 (deps (:t sequence.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target shebang.ml.corrected)
 (deps (:t shebang.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target shortcut_ext_attr.ml.corrected)
 (deps (:t shortcut_ext_attr.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target skip.ml.corrected)
 (deps (:t skip.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target source.ml.corrected)
 (deps (:t source.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target str_value.ml.corrected)
 (deps (:t str_value.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target string.ml.corrected)
 (deps (:t string.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target string_array.ml.corrected)
 (deps (:t string_array.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target string_wrapping.ml.corrected)
 (deps (:t string_wrapping.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target symbol.ml.corrected)
 (deps (:t symbol.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target tag_only.ml.corrected)
 (deps (:t tag_only.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target try_with_or_pattern.ml.corrected)
 (deps (:t try_with_or_pattern.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target tuple.ml.corrected)
 (deps (:t tuple.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target tuple_less_parens.ml.corrected)
 (deps (:t tuple_less_parens.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target tuple_type_parens.ml.corrected)
 (deps (:t tuple_type_parens.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target type_and_constraint.ml.corrected)
 (deps (:t type_and_constraint.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target type_annotations.ml.corrected)
 (deps (:t type_annotations.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target types.ml.corrected)
 (deps (:t types.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target unary.ml.corrected)
 (deps (:t unary.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target unicode.ml.corrected)
 (deps (:t unicode.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target verbose1.ml.corrected)
 (deps (:t verbose1.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target verbose2.ml.corrected)
 (deps (:t verbose2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target wrap_comments.ml.corrected)
 (deps (:t wrap_comments.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target wrap_comments_break.ml.corrected)
 (deps (:t wrap_comments_break.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))

(rule
 (alias runtest)
 (target wrapping_functor_args.ml.corrected)
 (deps (:t wrapping_functor_args.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} --ignore-docstrings %{t}))))
