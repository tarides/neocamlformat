open Document

let decorate token ~extension attrs ~later =
  let kw =
    match extension with
    | None -> token
    | Some ext ->
      let percent = pp_token ~after:token ~before:later PERCENT in
      token ^^ percent ^^ str ext
  in
  Attribute.attach_to_item ~spaces:0 kw attrs

