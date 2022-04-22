open Document
open Source_parsing
open Location

let to_doc ~loc t =
  string ~loc (Source_parsing.Source.print_tok t)

let pp_between_locs ~expect_failure start stop tok =
  let loc =
    Source_parsing.Source.loc_of_token_between ~expect_failure ~start ~stop tok
  in
  to_doc ~loc tok

let pp ?(expect_failure=false) ?inside ?before ?after tok =
  match inside, after, before with
  | (* Between two located documents. *)
    _,
    Some { doc = Located { loc = { loc_end = start; _ }; _ }; _ },
    Some { doc = Located { loc = { loc_start = stop; _ }; _ }; _ }
  | (* We know the range to search, and we know when to stop looking. *)
    Some { loc_start = start; _ },
    _,
    Some { doc = Located { loc = { loc_start = stop; _ }; _ }; _ }
  | (* We know the range to search, and we know where to start looking. *)
    Some { loc_end = stop; _ },
    Some { doc = Located { loc = { loc_end = start; _ }; _ }; _ },
    _
  | (* We know the range to search. *)
    Some { loc_start = start; loc_end = stop; }, _, _ ->
    pp_between_locs ~expect_failure start stop tok
  | None, _, _ ->
    invalid_arg "pp_token: need at least two positions"
