let fmt_file fn =
  let open Source_parsing in
  let open Printing in
  let ic = open_in fn in
  let b = Lexing.from_channel ic in
    if Filename.check_suffix fn "mli" then
      let sg = Parse_source.interface b in
      Print_source.Signature.pp sg
    else
      let st = Parse_source.implementation b in
      Print_source.Structure.pp st

open Cmdliner

let (let+) x f = Term.app (Term.const f) x
let (and+) t1 t2 = Term.(const (fun x y -> (x, y)) $ t1 $ t2)

let (let*) = Result.bind

let cmd =
  let open Printing.Options in
  let+ record_exp = Record.expression_arg
  and+ record_pat = Record.pattern_arg
  and+ record_all = Record.all_arg
  and+ width = Arg.(value & opt int 80 & info ["width"])
  and+ files = Arg.(value & pos_all file [] & info ~doc:"files to format" []) in
  let* () = Choice.setopts Record.[ expression_choice ] record_exp in
  let* () = Choice.setopts Record.[ pattern_choice ] record_pat in
  let* () = Choice.setopts Record.[ expression_choice; pattern_choice ] record_all in
  List.iter (fun fn ->
    let doc = fmt_file fn in
    PPrint.ToChannel.pretty 10. width stdout doc;
    print_newline ();
  ) files;
  Ok (flush stdout)

let info =
  Term.info "pp"

let () =
  Term.exit (Term.eval (cmd, info))
