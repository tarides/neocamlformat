
(rule
 (target align_cases.ml.corrected)
 (deps (:t align_cases.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps align_cases.ml.corrected))

(rule
 (target align_infix.ml.corrected)
 (deps (:t align_infix.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps align_infix.ml.corrected))

(rule
 (target apply.ml.corrected)
 (deps (:t apply.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps apply.ml.corrected))

(rule
 (target array.ml.corrected)
 (deps (:t array.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps array.ml.corrected))

(rule
 (target assignment_operator.ml.corrected)
 (deps (:t assignment_operator.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps assignment_operator.ml.corrected))

(rule
 (target attribute_and_expression.ml.corrected)
 (deps (:t attribute_and_expression.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps attribute_and_expression.ml.corrected))

(rule
 (target attributes.ml.corrected)
 (deps (:t attributes.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps attributes.ml.corrected))

(rule
 (target break_before_in.ml.corrected)
 (deps (:t break_before_in.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps break_before_in.ml.corrected))

(rule
 (target break_cases.ml.corrected)
 (deps (:t break_cases.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps break_cases.ml.corrected))

(rule
 (target break_fun_decl.ml.corrected)
 (deps (:t break_fun_decl.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps break_fun_decl.ml.corrected))

(rule
 (target break_record.ml.corrected)
 (deps (:t break_record.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps break_record.ml.corrected))

(rule
 (target break_separators.ml.corrected)
 (deps (:t break_separators.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps break_separators.ml.corrected))

(rule
 (target break_sequence_before.ml.corrected)
 (deps (:t break_sequence_before.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps break_sequence_before.ml.corrected))

(rule
 (target break_string_literals.ml.corrected)
 (deps (:t break_string_literals.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps break_string_literals.ml.corrected))

(rule
 (target break_struct.ml.corrected)
 (deps (:t break_struct.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps break_struct.ml.corrected))

(rule
 (target cinaps.ml.corrected)
 (deps (:t cinaps.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps cinaps.ml.corrected))

(rule
 (target cmdline_override.ml.corrected)
 (deps (:t cmdline_override.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps cmdline_override.ml.corrected))

(rule
 (target cmdline_override2.ml.corrected)
 (deps (:t cmdline_override2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps cmdline_override2.ml.corrected))

(rule
 (target comment_breaking.ml.corrected)
 (deps (:t comment_breaking.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comment_breaking.ml.corrected))

(rule
 (target comment_header.ml.corrected)
 (deps (:t comment_header.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comment_header.ml.corrected))

(rule
 (target comment_in_empty.ml.corrected)
 (deps (:t comment_in_empty.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comment_in_empty.ml.corrected))

(rule
 (target comment_in_modules.ml.corrected)
 (deps (:t comment_in_modules.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comment_in_modules.ml.corrected))

(rule
 (target comment_last.ml.corrected)
 (deps (:t comment_last.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comment_last.ml.corrected))

(rule
 (target comment_sparse.ml.corrected)
 (deps (:t comment_sparse.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comment_sparse.ml.corrected))

(rule
 (target comments.ml.corrected)
 (deps (:t comments.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comments.ml.corrected))

(rule
 (target comments_args.ml.corrected)
 (deps (:t comments_args.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comments_args.ml.corrected))

(rule
 (target comments_around_disabled.ml.corrected)
 (deps (:t comments_around_disabled.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comments_around_disabled.ml.corrected))

(rule
 (target comments_in_record.ml.corrected)
 (deps (:t comments_in_record.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps comments_in_record.ml.corrected))

(rule
 (target compact_lists_arrays.ml.corrected)
 (deps (:t compact_lists_arrays.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps compact_lists_arrays.ml.corrected))

(rule
 (target custom_list.ml.corrected)
 (deps (:t custom_list.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps custom_list.ml.corrected))

(rule
 (target disabled.ml.corrected)
 (deps (:t disabled.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps disabled.ml.corrected))

(rule
 (target disambiguate.ml.corrected)
 (deps (:t disambiguate.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps disambiguate.ml.corrected))

(rule
 (target doc_comments.ml.corrected)
 (deps (:t doc_comments.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps doc_comments.ml.corrected))

(rule
 (target doc_comments_padding.ml.corrected)
 (deps (:t doc_comments_padding.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps doc_comments_padding.ml.corrected))

(rule
 (target empty.ml.corrected)
 (deps (:t empty.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps empty.ml.corrected))

(rule
 (target empty_ml.ml.corrected)
 (deps (:t empty_ml.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps empty_ml.ml.corrected))

(rule
 (target error1.ml.corrected)
 (deps (:t error1.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps error1.ml.corrected))

(rule
 (target error2.ml.corrected)
 (deps (:t error2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps error2.ml.corrected))

(rule
 (target error3.ml.corrected)
 (deps (:t error3.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps error3.ml.corrected))

(rule
 (target error4.ml.corrected)
 (deps (:t error4.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps error4.ml.corrected))

(rule
 (target exceptions.ml.corrected)
 (deps (:t exceptions.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps exceptions.ml.corrected))

(rule
 (target exp_grouping.ml.corrected)
 (deps (:t exp_grouping.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps exp_grouping.ml.corrected))

(rule
 (target exp_record.ml.corrected)
 (deps (:t exp_record.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps exp_record.ml.corrected))

(rule
 (target expect_test.ml.corrected)
 (deps (:t expect_test.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps expect_test.ml.corrected))

(rule
 (target extensions.ml.corrected)
 (deps (:t extensions.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps extensions.ml.corrected))

(rule
 (target field.ml.corrected)
 (deps (:t field.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps field.ml.corrected))

(rule
 (target first_class_module.ml.corrected)
 (deps (:t first_class_module.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps first_class_module.ml.corrected))

(rule
 (target floating_doc.ml.corrected)
 (deps (:t floating_doc.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps floating_doc.ml.corrected))

(rule
 (target for_while.ml.corrected)
 (deps (:t for_while.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps for_while.ml.corrected))

(rule
 (target format_invalid_files.ml.corrected)
 (deps (:t format_invalid_files.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps format_invalid_files.ml.corrected))

(rule
 (target fun_decl.ml.corrected)
 (deps (:t fun_decl.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps fun_decl.ml.corrected))

(rule
 (target function_indent.ml.corrected)
 (deps (:t function_indent.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps function_indent.ml.corrected))

(rule
 (target functor.ml.corrected)
 (deps (:t functor.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps functor.ml.corrected))

(rule
 (target funsig.ml.corrected)
 (deps (:t funsig.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps funsig.ml.corrected))

(rule
 (target gadt.ml.corrected)
 (deps (:t gadt.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps gadt.ml.corrected))

(rule
 (target generative.ml.corrected)
 (deps (:t generative.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps generative.ml.corrected))

(rule
 (target index_op.ml.corrected)
 (deps (:t index_op.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps index_op.ml.corrected))

(rule
 (target infix_arg_grouping.ml.corrected)
 (deps (:t infix_arg_grouping.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps infix_arg_grouping.ml.corrected))

(rule
 (target infix_bind.ml.corrected)
 (deps (:t infix_bind.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps infix_bind.ml.corrected))

(rule
 (target infix_precedence.ml.corrected)
 (deps (:t infix_precedence.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps infix_precedence.ml.corrected))

(rule
 (target invalid.ml.corrected)
 (deps (:t invalid.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps invalid.ml.corrected))

(rule
 (target invalid_docstring.ml.corrected)
 (deps (:t invalid_docstring.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps invalid_docstring.ml.corrected))

(rule
 (target issue114.ml.corrected)
 (deps (:t issue114.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue114.ml.corrected))

(rule
 (target issue289.ml.corrected)
 (deps (:t issue289.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue289.ml.corrected))

(rule
 (target issue48.ml.corrected)
 (deps (:t issue48.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue48.ml.corrected))

(rule
 (target issue51.ml.corrected)
 (deps (:t issue51.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue51.ml.corrected))

(rule
 (target issue57.ml.corrected)
 (deps (:t issue57.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue57.ml.corrected))

(rule
 (target issue60.ml.corrected)
 (deps (:t issue60.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue60.ml.corrected))

(rule
 (target issue77.ml.corrected)
 (deps (:t issue77.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue77.ml.corrected))

(rule
 (target issue85.ml.corrected)
 (deps (:t issue85.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue85.ml.corrected))

(rule
 (target issue89.ml.corrected)
 (deps (:t issue89.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps issue89.ml.corrected))

(rule
 (target ite.ml.corrected)
 (deps (:t ite.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps ite.ml.corrected))

(rule
 (target js_source.ml.corrected)
 (deps (:t js_source.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps js_source.ml.corrected))

(rule
 (target kw_extentions.ml.corrected)
 (deps (:t kw_extentions.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps kw_extentions.ml.corrected))

(rule
 (target label_option_default_args.ml.corrected)
 (deps (:t label_option_default_args.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps label_option_default_args.ml.corrected))

(rule
 (target lazy.ml.corrected)
 (deps (:t lazy.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps lazy.ml.corrected))

(rule
 (target let_binding.ml.corrected)
 (deps (:t let_binding.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps let_binding.ml.corrected))

(rule
 (target let_in_constr.ml.corrected)
 (deps (:t let_in_constr.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps let_in_constr.ml.corrected))

(rule
 (target let_module.ml.corrected)
 (deps (:t let_module.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps let_module.ml.corrected))

(rule
 (target list.ml.corrected)
 (deps (:t list.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps list.ml.corrected))

(rule
 (target loc_stack.ml.corrected)
 (deps (:t loc_stack.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps loc_stack.ml.corrected))

(rule
 (target locally_abtract_types.ml.corrected)
 (deps (:t locally_abtract_types.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps locally_abtract_types.ml.corrected))

(rule
 (target margin_80.ml.corrected)
 (deps (:t margin_80.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps margin_80.ml.corrected))

(rule
 (target match.ml.corrected)
 (deps (:t match.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps match.ml.corrected))

(rule
 (target match2.ml.corrected)
 (deps (:t match2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps match2.ml.corrected))

(rule
 (target match_indent.ml.corrected)
 (deps (:t match_indent.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps match_indent.ml.corrected))

(rule
 (target max_indent.ml.corrected)
 (deps (:t max_indent.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps max_indent.ml.corrected))

(rule
 (target module.ml.corrected)
 (deps (:t module.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps module.ml.corrected))

(rule
 (target module_anonymous.ml.corrected)
 (deps (:t module_anonymous.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps module_anonymous.ml.corrected))

(rule
 (target module_attributes.ml.corrected)
 (deps (:t module_attributes.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps module_attributes.ml.corrected))

(rule
 (target module_item_spacing.ml.corrected)
 (deps (:t module_item_spacing.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps module_item_spacing.ml.corrected))

(rule
 (target module_type.ml.corrected)
 (deps (:t module_type.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps module_type.ml.corrected))

(rule
 (target monadic_binding.ml.corrected)
 (deps (:t monadic_binding.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps monadic_binding.ml.corrected))

(rule
 (target multi_index_op.ml.corrected)
 (deps (:t multi_index_op.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps multi_index_op.ml.corrected))

(rule
 (target need_format.ml.corrected)
 (deps (:t need_format.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps need_format.ml.corrected))

(rule
 (target new.ml.corrected)
 (deps (:t new.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps new.ml.corrected))

(rule
 (target object.ml.corrected)
 (deps (:t object.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps object.ml.corrected))

(rule
 (target object_type.ml.corrected)
 (deps (:t object_type.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps object_type.ml.corrected))

(rule
 (target ocp_indent_compat.ml.corrected)
 (deps (:t ocp_indent_compat.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps ocp_indent_compat.ml.corrected))

(rule
 (target ocp_indent_options.ml.corrected)
 (deps (:t ocp_indent_options.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps ocp_indent_options.ml.corrected))

(rule
 (target open.ml.corrected)
 (deps (:t open.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps open.ml.corrected))

(rule
 (target open_types.ml.corrected)
 (deps (:t open_types.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps open_types.ml.corrected))

(rule
 (target option.ml.corrected)
 (deps (:t option.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps option.ml.corrected))

(rule
 (target parens_tuple_patterns.ml.corrected)
 (deps (:t parens_tuple_patterns.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps parens_tuple_patterns.ml.corrected))

(rule
 (target precedence.ml.corrected)
 (deps (:t precedence.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps precedence.ml.corrected))

(rule
 (target prefix_infix.ml.corrected)
 (deps (:t prefix_infix.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps prefix_infix.ml.corrected))

(rule
 (target print_config.ml.corrected)
 (deps (:t print_config.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps print_config.ml.corrected))

(rule
 (target profiles.ml.corrected)
 (deps (:t profiles.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps profiles.ml.corrected))

(rule
 (target profiles2.ml.corrected)
 (deps (:t profiles2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps profiles2.ml.corrected))

(rule
 (target protected_object_types.ml.corrected)
 (deps (:t protected_object_types.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps protected_object_types.ml.corrected))

(rule
 (target record.ml.corrected)
 (deps (:t record.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps record.ml.corrected))

(rule
 (target record_punning.ml.corrected)
 (deps (:t record_punning.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps record_punning.ml.corrected))

(rule
 (target reformat_string.ml.corrected)
 (deps (:t reformat_string.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps reformat_string.ml.corrected))

(rule
 (target refs.ml.corrected)
 (deps (:t refs.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps refs.ml.corrected))

(rule
 (target remove_extra_parens.ml.corrected)
 (deps (:t remove_extra_parens.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps remove_extra_parens.ml.corrected))

(rule
 (target revapply_ext.ml.corrected)
 (deps (:t revapply_ext.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps revapply_ext.ml.corrected))

(rule
 (target send.ml.corrected)
 (deps (:t send.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps send.ml.corrected))

(rule
 (target sequence.ml.corrected)
 (deps (:t sequence.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps sequence.ml.corrected))

(rule
 (target shebang.ml.corrected)
 (deps (:t shebang.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps shebang.ml.corrected))

(rule
 (target shortcut_ext_attr.ml.corrected)
 (deps (:t shortcut_ext_attr.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps shortcut_ext_attr.ml.corrected))

(rule
 (target skip.ml.corrected)
 (deps (:t skip.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps skip.ml.corrected))

(rule
 (target source.ml.corrected)
 (deps (:t source.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps source.ml.corrected))

(rule
 (target str_value.ml.corrected)
 (deps (:t str_value.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps str_value.ml.corrected))

(rule
 (target string.ml.corrected)
 (deps (:t string.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps string.ml.corrected))

(rule
 (target string_array.ml.corrected)
 (deps (:t string_array.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps string_array.ml.corrected))

(rule
 (target string_wrapping.ml.corrected)
 (deps (:t string_wrapping.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps string_wrapping.ml.corrected))

(rule
 (target symbol.ml.corrected)
 (deps (:t symbol.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps symbol.ml.corrected))

(rule
 (target tag_only.ml.corrected)
 (deps (:t tag_only.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps tag_only.ml.corrected))

(rule
 (target try_with_or_pattern.ml.corrected)
 (deps (:t try_with_or_pattern.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps try_with_or_pattern.ml.corrected))

(rule
 (target tuple.ml.corrected)
 (deps (:t tuple.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps tuple.ml.corrected))

(rule
 (target tuple_less_parens.ml.corrected)
 (deps (:t tuple_less_parens.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps tuple_less_parens.ml.corrected))

(rule
 (target tuple_type_parens.ml.corrected)
 (deps (:t tuple_type_parens.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps tuple_type_parens.ml.corrected))

(rule
 (target type_and_constraint.ml.corrected)
 (deps (:t type_and_constraint.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps type_and_constraint.ml.corrected))

(rule
 (target type_annotations.ml.corrected)
 (deps (:t type_annotations.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps type_annotations.ml.corrected))

(rule
 (target types.ml.corrected)
 (deps (:t types.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps types.ml.corrected))

(rule
 (target unary.ml.corrected)
 (deps (:t unary.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps unary.ml.corrected))

(rule
 (target unicode.ml.corrected)
 (deps (:t unicode.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps unicode.ml.corrected))

(rule
 (target verbose1.ml.corrected)
 (deps (:t verbose1.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps verbose1.ml.corrected))

(rule
 (target verbose2.ml.corrected)
 (deps (:t verbose2.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps verbose2.ml.corrected))

(rule
 (target wrap_comments.ml.corrected)
 (deps (:t wrap_comments.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps wrap_comments.ml.corrected))

(rule
 (target wrap_comments_break.ml.corrected)
 (deps (:t wrap_comments_break.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps wrap_comments_break.ml.corrected))

(rule
 (target wrapping_functor_args.ml.corrected)
 (deps (:t wrapping_functor_args.ml))
 (action
   (with-stdout-to %{target}
     (run %{bin:neocamlformat} %{t}))))

(alias
 (name runtest)
 (deps wrapping_functor_args.ml.corrected))
