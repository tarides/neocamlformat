open Source_parsing.Location

val pp_item_attr : (Source_parsing.Source_tree.attribute -> Document.t) ref

type t = {
  lhs : Document.t;
  params: Document.t list loc;
  constr: Document.t option;
  coerce: Document.t option;
  rhs : Document.t option;
}

type binding_kw = {
  token: Document.t;
  extension: string loc option;
  attrs: Source_parsing.Source_tree.attributes;
  modifier: Source_parsing.Parser.token option;
}

val pp
  : ?binder:Source_parsing.Parser.token
  -> ?keyword:binding_kw
  -> t
  -> Document.t

val pp_simple
  : ?binder:Source_parsing.Parser.token
  -> keyword:Document.t
  -> Document.t
  -> Document.t
  -> Document.t

module Module : sig
  type constraint_ =
    | None
    | Sig of Document.t
    | Mty of Document.t

  type body =
    | Items of Document.t
    | Generic of Document.t

  type t = {
    name : Document.t;
    params: Document.t list loc;
    constr: constraint_;
    body:  body;
    attributes: Document.t;
  }

  type context = Sig | Struct

  val pp
    : keyword:Document.t
    -> context:context
    -> t
    -> Document.t
end
