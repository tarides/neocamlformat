open Source_parsing
open Asttypes
open Source_tree

val ends_in_obj : core_type -> bool
val starts_with_obj : core_type -> bool
val pp : core_type -> Document.t
val pp_param : (arg_label * core_type) -> Document.t

module Package_type : sig
  val pp : package_type -> Document.t
end

