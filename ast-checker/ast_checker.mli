(** Check that two strings parse to the same AST (modulo locations) *)

val check_same_ast : impl:bool -> string -> string -> bool
