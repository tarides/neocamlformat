open Cmdliner

module Wrappable = struct
  type t = Fit_or_vertical | Wrap

  let parse = function
    | "fit-or-vertical" -> Ok Fit_or_vertical
    | "wrap" -> Ok Wrap
    | s ->
      let msg = Printf.sprintf "accepted fit-or-vertical | wrap, got %S" s in
      Error (`Msg msg)

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Fit_or_vertical -> "fit-or-vertical"
       | Wrap -> "docked")

  let t = Arg.conv (parse, print)
end

module Parenthesing = struct
  type t = Parens | Begin_end

  let parse = function
    | "parens"
    | "parentheses" -> Ok Parens
    | "begin-end"
    | "begin" -> Ok Begin_end
    | s ->
      let msg =
        Printf.sprintf
          "accepted parens | parentheses | begin | begin-end, got %S" s
      in
      Error (`Msg msg)

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Parens -> "parentheses"
       | Begin_end -> "begin-end")

  let t = Arg.conv (parse, print)
end

module Situations = struct
  type t = When_needed | Always

  let parse = function
    | "always" -> Ok Always
    | "when-needed" -> Ok When_needed
    | s ->
      let msg =
        Printf.sprintf
          "accepted always | when-needed, got %S" s
      in
      Error (`Msg msg)

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Always -> "always"
       | When_needed -> "when_needed")

  let t = Arg.conv (parse, print)
end

let (:=) ref term =
  Term.(const (fun x -> ref := x) $ term)

module Record = struct
  let expression = ref Wrappable.Fit_or_vertical
  let expression_cmd =
    let open Arg in
    let info = info ~doc:"formatting of record expressions" ["record-expr"] in
    expression := value & opt Wrappable.t Fit_or_vertical info

  let pattern = ref Wrappable.Wrap
  let pattern_cmd =
    let open Arg in
    let info = info ~doc:"formatting of record patterns" ["record-patt"] in
    pattern := value & opt Wrappable.t Wrap info
end

module Match = struct
  let parens_style_choice = ref Parenthesing.Begin_end
  let parens_style_cmd =
    let open Arg in
    let info =
      info ~doc:"style of parenthesing to use around match expressions"
        ["match-parens-style" ;"match-parenthezing-style"]
    in
    parens_style_choice := value & opt Parenthesing.t Begin_end info

  let parenthesing_situations_choice = ref Situations.When_needed
  let parens_situations_cmd =
    let open Arg in
    let info =
      info ~doc:"when to add parentheses around match expressions"
        ["match-parenthezing"]
    in
    parenthesing_situations_choice := value & opt Situations.t When_needed info
end
