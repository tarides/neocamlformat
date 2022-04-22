open Source_parsing

module Rhs : sig
  type t =
    | Absent
    | Regular of Document.t
    | Two_parts of Document.t * Document.t

  val of_opt : ('a -> Document.t) -> 'a option -> t
end


type t =
  {
    lhs : Document.t;
    params : Document.t list;
    constr : Document.t option;
    coerce : Document.t option;
    rhs : Rhs.t
  }

val pp
  :  ?binder:Parser.token
  -> ?keyword:Document.t
  -> t
  -> Document.t

val pp_simple
  :  ?binder:Parser.token
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

  type t =
    {
      name : Document.t;
      params : Document.t list;
      constr : constraint_;
      body : body;
      attributes : Document.t
    }

  type context =
    | Sig
    | Struct

  val pp
    : keyword:Document.t -> loc:Location.t -> context:context -> t -> Document.t
end

