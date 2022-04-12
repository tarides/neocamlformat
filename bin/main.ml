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

type fmt_file_error =
  | Invalid_input of exn
  | Ast_changed
  | Invalid_generated_file of exn
  | Internal_error of string * exn

exception Fmt_file_error of fmt_file_error

let fmt_file fn =
  let source = read_file fn in
  let intf = Filename.check_suffix fn "mli" in
  let fmted =
    let open Source_parsing in
    let open Printing in
    Location.input_name := fn;
    Source.source := source;
    let b = Lexing.from_string source in
    let pp parse print =
      match parse b with
      | exception ((Syntaxerr.Error _ | Lexer.Error _ | Location.Error _) as exn) ->
         raise (Fmt_file_error (Invalid_input exn))
      | sg ->
         let comments = Lexer.comments () in
         let () =
           try Comments.init comments with e ->
             raise (Fmt_file_error (Internal_error ("comments-init", e)))
         in
         try print sg with
         | e ->
           let bt = Printexc.get_backtrace () in
           Printf.eprintf "BACKTRACE: %s\n%!" bt;
           raise (Fmt_file_error (Internal_error ("printing", e)))
    in
    let doc =
      if intf
      then pp Parse.interface Print_source.interface
      else pp Parse.implementation Print_source.implementation
    in
    Comments.report_remaining ();
    let buf = Buffer.create (String.length source) in
    PPrint.ToBuffer.pretty 10. !Options.width buf doc;
    Buffer.to_bytes buf |> Bytes.to_string
  in
  (try
     if not (Ast_checker.check_same_ast ~impl:(not intf) source fmted)
     then raise (Fmt_file_error Ast_changed)
   with
   | e ->
      let oc = open_out "/tmp/out.txt" in
      output_string oc fmted;
      close_out oc;
      match e with
      | (Syntaxerr.Error _ | Lexer.Error _ | Location.Error _) as e ->
         raise (Fmt_file_error (Invalid_generated_file e))
      | Fmt_file_error _ as e -> raise e
      | e -> raise (Fmt_file_error (Internal_error ("check_same_ast", e))));
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
  and+ () = If_branch.parens_style_cmd
  and+ () = If_branch.parens_situations_cmd
  and+ () = Applications.layout_cmd
  and+ () = width := Arg.(value & opt int 80 & info ["w"; "width"])
  and+ files = Arg.(value & pos_all file [] & info ~doc:"files to format" [])
  and+ ignore_docstrings = Arg.(value & flag & info ["ignore-docstrings"])
  and+ quiet = Arg.(value & flag & info ["quiet"])
  and+ inplace = Arg.(value & flag & info ["i"; "inplace"])
  in
  Ast_checker.ignore_docstrings.contents <- ignore_docstrings;
  let ok = ref true in
  List.iter (fun fn ->
      match fmt_file fn with
      | exception (Fmt_file_error e) ->
         begin match e with
         | Invalid_input e ->
            if not quiet then Source_parsing.Location.report_exception Format.err_formatter e
         | Internal_error (txt, e) ->
            Format.eprintf "neocamlformat: (%S) internal error:%s:@;%s@." fn txt
              (Printexc.to_string e)
         | Ast_changed ->
            Format.eprintf "neocamlformat: AST of %S changed by formater@." fn
         | Invalid_generated_file e ->
            Format.eprintf "neocamlformat: formated %S doesn't parse:@.%a@."
              fn Location.report_exception e;
         end;
         ok.contents <- false
      | exception e ->
         Format.eprintf "neocamlformat: (%S) uncaught internal error:@;%s@." fn
           (Printexc.to_string e)
      | fmted ->
         if not inplace then (
           print_string fmted;
           print_newline ()
         ) else (
           let tmpfile = Filename.temp_file "neocamlformat" "ml" in
           let oc = open_out tmpfile in
           output_string oc fmted;
           output_string oc "\n";
           flush oc;
           close_out oc;
           ignore (Sys.command ("mv " ^ tmpfile ^ " " ^ fn))
         )
    ) files;
  flush stdout;
  if not !ok then exit 1

let info =
  Cmd.info ~exits:Cmd.Exit.defaults "neocamlformat"

let () =
  Stdlib.exit (Cmd.eval @@ Cmd.v info cmd)
