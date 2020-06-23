let read_file fn =
  let ic = open_in fn in
  let chunk_size =
    (* taken from janestreet's stdio, where they say:
       > We use 65536 because that is the size of OCaml's IO buffers. *)
    65536
  in
  let buffer = Buffer.create chunk_size in
  let rec loop () =
    Buffer.add_channel buffer ic chunk_size;
    loop ()
  in
  try loop ()
  with End_of_file -> Buffer.contents buffer

let fmt_file ~width fn =
  let source = read_file fn in
  let intf = Filename.check_suffix fn "mli" in
  let fmted =
    let open Source_parsing in
    let open Printing in
    Location.input_name := fn;
    Source.source := source;
    let b = Lexing.from_string source in
    let doc =
      if intf then
        match Parse.interface b with
        | exception (Syntaxerr.Error _ as exn) ->
          Location.report_exception Format.err_formatter exn;
          exit 1
        | sg ->
          let _ = Comments.init () in
          Print_source.interface sg
      else
        match Parse.implementation b with
        | exception (Syntaxerr.Error _ as exn) ->
          Location.report_exception Format.err_formatter exn;
          exit 1
        | str ->
          let _ = Comments.init () in
          Print_source.implementation str
    in
    Comments.report_remaining ();
    let buf = Buffer.create (String.length source) in
    PPrint.ToBuffer.pretty 10. width buf doc;
    Buffer.to_bytes buf |> Bytes.to_string
  in
  begin try
    assert (Ast_checker.check_same_ast ~impl:(not intf) source fmted);
  with e ->
    let oc = open_out "/tmp/out.txt" in
    output_string oc fmted;
    close_out oc;
    begin match e with
    | Syntaxerr.Error _ ->
      Format.eprintf "neocamlformat: formated file doesn't parse:@.%a@."
        Location.report_exception e
    | Assert_failure _ ->
      Format.eprintf "neocamlformat: AST changed by formater@."
    | _ ->
      Format.eprintf "neocamlformat: internal error:@;%s@."
        (Printexc.to_string e)
    end;
    exit 2
  end;
  fmted

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
  and+ ignore_docstrings = Arg.(value & flag & info ["ignore-docstrings"])
  in
  Ast_checker.ignore_docstrings.contents <- ignore_docstrings;
  List.iter (fun fn ->
    let fmted = fmt_file ~width fn in
    print_string fmted;
    print_newline ();
  ) files;
  Ok (flush stdout)

let info =
  Term.info "neocamlformat"

let () =
  Term.exit (Term.eval (cmd, info))
