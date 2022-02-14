open Source_parsing

include module type of struct include Longident end

val pp : t -> Document.t
val pp_ident : string Location.loc -> Document.t
