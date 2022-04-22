open Document

let decorate token ~extension attrs =
  let kw =
    match extension with
    | None -> token
    | Some ext ->
      let ext = str ext in
      let percent = Token.pp ~after:token ~before:ext PERCENT in
      token ^^ percent ^^ ext
  in
  Attribute.attach_to_item ~spaces:0 kw attrs
