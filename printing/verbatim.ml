open PPrint
open Source_parsing

let can_shift_lines comment init_col s =
  let rec all_blank ~from ~to_ =
    from = to_ || (String.get s from = ' ' && all_blank ~from:(from + 1) ~to_)
  in
  let rec aux i =
    match String.index_from_opt s i '\n' with
    | None -> true
    | Some j ->
      let from = j + 1 in
      let to_ = from + init_col in
      begin
        try (comment || String.get s (j - 1) = '\\') && all_blank ~from ~to_
        with Invalid_argument _ (* out of bounds *) ->
          false
      end &&
        aux to_
  in
  aux 0

class verbatim_string ?(comment=false) ?adjust_indent s : PPrint.custom =
  let req = if String.contains s '\n' then infinity else String.length s in
  let init_col =
    Option.bind adjust_indent (fun (loc : Location.t) ->
      let init_col = loc.loc_start.pos_cnum - loc.loc_start.pos_bol in
      if can_shift_lines comment init_col s then Some init_col else None
    )
  in
  object
    method requirement =
      req

    method pretty output state _ flattening =
      let shift =
        match init_col with
        | None -> `Done
        | Some init_col ->
          let shift = state.column - init_col in
          if shift = 0 then
            `Done
          else if shift > 0 then
            `Positive (shift, String.init shift (Fun.const ' '))
          else
            `Negative (abs shift)
      in
      let rec aux i =
        match String.index_from_opt s i '\n' with
        | Some j ->
          assert (not flattening);
          if j - i > 0 then output#substring s i (j - i);
          output#char '\n';
          state.line <- state.line + 1;
          let j =
            match shift with
            | `Done ->
              state.column <- 0;
              j
            | `Positive (len, str) ->
              output#substring str 0 len;
              state.column <- len;
              j
            | `Negative i ->
              state.column <- 0;
              j + i
          in
          aux (j + 1)
        | None ->
          let len = String.length s - i in
          output#substring s i len;
          state.column <- state.column + len
      in
      aux 0

    method compact output =
      assert (req != infinity);
      output#substring s 0 req
  end

let pp_string ?comment ?adjust_indent s =
  custom (new verbatim_string ?comment ?adjust_indent s)

