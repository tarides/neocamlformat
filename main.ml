let () =
  let fn = Sys.argv.(1) in
  let width =
    try int_of_string (Sys.argv.(2))
    with _ -> 80
  in
  let ic = open_in fn in
  let b = Lexing.from_channel ic in
  let doc =
    if Filename.check_suffix fn "mli" then
      let sg = Parse_source.interface b in
      Pparsetree.Signature.pp sg
    else
      let st = Parse_source.implementation b in
      Pparsetree.Structure.pp st
  in
  PPrint.ToChannel.pretty 10. width stdout doc;
  print_newline ();
  flush stdout
