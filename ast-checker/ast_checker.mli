(** Check that two strings parse to the same AST (modulo locations) *)

val ignore_docstrings : bool ref

val check_same_ast : impl:bool -> string -> string -> bool
