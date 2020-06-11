open Source_parsing.Location

type t = {
  lhs : Document.t;
  params: Document.t list loc;
  constr: Document.t option;
  coerce: Document.t option;
  rhs : Document.t option;
}

val pp
  : ?binder:Tokens.t
  -> ?keyword:Document.t
  -> t
  -> Document.t

val pp_simple
  : ?binder:Tokens.t
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
