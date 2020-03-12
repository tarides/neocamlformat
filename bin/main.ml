let fmt_file fn =
  let open Source_parsing in
  let open Printing in
  let ic = open_in fn in
  Location.input_name := fn;
  let b = Lexing.from_channel ic in
  let doc =
    if Filename.check_suffix fn "mli" then
      match Parse_source.interface b with
      | [] -> PPrint.empty
      | si :: sg ->
        let _ = Comments.init () in
        let doc = Print_source.Signature.pp_nonempty si sg in
        doc.txt
    else
      match Parse_source.implementation b with
      | [] -> PPrint.empty
      | si :: st ->
        let _ = Comments.init () in
        let doc = Print_source.Structure.pp_nonempty si st in
        doc.txt
  in
  Comments.report_remaining ();
  doc

open Cmdliner

let (let+) x f = Term.app (Term.const f) x
let (and+) t1 t2 = Term.(const (fun x y -> (x, y)) $ t1 $ t2)

let cmd =
  let open Printing.Options in
  let+ () = Record.expression_cmd
  and+ () = Record.pattern_cmd
  and+ () = Match.parens_style_cmd
  and+ () = Match.parens_situations_cmd
  and+ () = Cases.body_indent_cmd
  and+ () = Cases.body_on_separate_line_cmd
  and+ width = Arg.(value & opt int 80 & info ["w"; "width"])
  and+ files = Arg.(value & pos_all file [] & info ~doc:"files to format" [])
  in
  List.iter (fun fn ->
    let doc = fmt_file fn in
    PPrint.ToChannel.pretty 10. width stdout doc;
    print_newline ();
  ) files;
  Ok (flush stdout)

let info =
  Term.info "neocamlformat"

let () =
  Term.exit (Term.eval (cmd, info))
