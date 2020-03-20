type t = string

let source = ref ""

let source_between (start : Lexing.position) (stop : Lexing.position) =
  let pos = start.pos_cnum in
  let len = stop.pos_cnum - start.pos_cnum in
  begin try
      assert (pos > 0);
      assert (len > 0);
    with exn ->
      Format.eprintf "source_between %d:%d -- %d:%d@."
        start.pos_lnum (start.pos_cnum - start.pos_bol)
        stop.pos_lnum (stop.pos_cnum - stop.pos_bol);
      raise exn
  end;
  String.sub !source pos len

let lexbuf_set_pos lexbuf pos =
  lexbuf.Lexing.lex_abs_pos <- pos.Lexing.pos_cnum ;
  lexbuf.lex_curr_p <- pos

let loc_of_token_between ~start ~stop token =
  let sub = source_between start stop in
  let lexbuf = Lexing.from_string sub in
  Lexer.init ();
  lexbuf_set_pos lexbuf start;
  let rec loop () =
    match Lexer.token lexbuf with
    | Parser.EOF -> raise Not_found
    | tok when tok = token -> Location.curr lexbuf
    | _ -> loop ()
  in
  loop ()
