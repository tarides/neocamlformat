let () =
  (*
  let ic = open_in (Sys.argv.(1)) in
  let b = Lexing.from_channel ic in
  let _ = Parse.implementation b in
  *)
  let _ = Pparsetree.Polymorphic_variant_tag.pp (Location.mknoloc "test") in
  ()
