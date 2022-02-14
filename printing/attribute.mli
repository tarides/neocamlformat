open Source_parsing.Source_tree

type kind =
  | Free_floating
  | Attached_to_structure_item
  | Attached_to_item

val is_non_doc : attribute -> bool
val has_non_doc : attributes -> bool

val pp : kind -> attribute -> Document.t

val attach : ?spaces:int -> kind -> Document.t -> attributes -> Document.t
val attach_to_item : ?spaces:int -> Document.t -> attributes -> Document.t
val attach_to_top_item : Document.t -> attributes -> Document.t

val extract_text
  : item_start_pos:Lexing.position -> attributes -> attributes * attributes
val prepend_text : attributes -> Document.t -> Document.t list

module Payload : sig
  val pp_after : tag:Document.t -> payload -> Document.t

  (* To be filled in to close the recursion! *)
  val pp_struct
    : (structure_item -> structure_item list -> Document.t) ref

  val struct_ends_in_obj
    : (structure_item list -> bool) ref

  val pp_sig
    : (signature_item -> signature_item list -> Document.t) ref

  val sig_ends_in_obj
    : (signature_item list -> bool) ref

  val pp_core_type
    : (core_type -> Document.t) ref

  val ct_ends_in_obj
    : (core_type -> bool) ref

  val pp_expression
    : (expression -> Document.t) ref

  val pp_pattern
    : (pattern -> Document.t) ref

end
