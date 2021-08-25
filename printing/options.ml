open Cmdliner

let accepted_values print ppf values =
  let open Format in
  fprintf ppf "accepted values: %a"
    (pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf " | ") print)
    values

let one_of print ppf values =
  let open Format in
  let wrapped fmt = Format.fprintf fmt "`%a'" print in
  fprintf ppf "The value $(docv) must be one of %a"
    (pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf ", ") wrapped)
    values

module Wrappable = struct
  type t = Fit_or_vertical | Wrap
  let all = [ Fit_or_vertical; Wrap ]

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Fit_or_vertical -> "fit-or-vertical"
       | Wrap -> "wrap")

  let parse = function
    | "fit-or-vertical" -> Ok Fit_or_vertical
    | "wrap" -> Ok Wrap
    | s ->
      let msg = Format.asprintf "%a, got %S" (accepted_values print) all s in
      Error (`Msg msg)

  let t = Arg.conv (parse, print)
end

module Smartly_wrappable = struct
  type t = Fit_or_vertical | Wrap | Smart
  let all = [ Fit_or_vertical; Smart; Wrap]

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Fit_or_vertical -> "fit-or-vertical"
       | Wrap -> "wrap"
       | Smart -> "smart")

  let parse = function
    | "fit-or-vertical" -> Ok Fit_or_vertical
    | "wrap" -> Ok Wrap
    | "smart" -> Ok Smart
    | s ->
      let msg = Format.asprintf "%a, got %S" (accepted_values print) all s in
      Error (`Msg msg)

  let t = Arg.conv (parse, print)
end

module Parenthesing = struct
  type t = Parens | Begin_end
  let all = [ Parens; Begin_end ]

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Parens -> "parentheses"
       | Begin_end -> "begin-end")

  let parse = function
    | "parens"
    | "parentheses" -> Ok Parens
    | "begin-end"
    | "begin" -> Ok Begin_end
    | s ->
      let msg = Format.asprintf "%a, got %S" (accepted_values print) all s in
      Error (`Msg msg)

  let t = Arg.conv (parse, print)
end

module Situations = struct
  type t = When_needed | Always
  let all = [ Always; When_needed ]

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Always -> "always"
       | When_needed -> "when_needed")

  let parse = function
    | "always" -> Ok Always
    | "when-needed" -> Ok When_needed
    | s ->
      let msg = Format.asprintf "%a, got %S" (accepted_values print) all s in
      Error (`Msg msg)

  let t = Arg.conv (parse, print)
end

let (:=) ref term =
  Term.(const (fun x -> ref := x) $ term)

let formatting_info = Arg.info ~docs:"FORMATTING OPTIONS" ~docv:"VAL"

module Record = struct
  let expression = ref Wrappable.Fit_or_vertical
  let expression_cmd =
    let open Arg in
    let doc =
      Format.asprintf
        "formatting of record expressions. %a"
        (one_of Wrappable.print) Wrappable.all
    in
    let info = formatting_info ~doc ["record-expr"] in
    expression := value & opt Wrappable.t Fit_or_vertical info

  let pattern = ref Wrappable.Wrap
  let pattern_cmd =
    let open Arg in
    let doc =
      Format.asprintf
        "formatting of record patterns. %a"
        (one_of Wrappable.print) Wrappable.all
    in
    let info = formatting_info ~doc ["record-patt"] in
    pattern := value & opt Wrappable.t Wrap info
end

module Match = struct
  let parens_style = ref Parenthesing.Begin_end
  let parens_style_cmd =
    let open Arg in
    let doc =
      Format.asprintf
        "style of parenthesing to use around match expressions. %a"
        (one_of Parenthesing.print) Parenthesing.all
    in
    let info =
      formatting_info ~doc
        ["match-parens-style" ;"match-parenthezing-style"]
    in
    parens_style := value & opt Parenthesing.t Begin_end info

  let parenthesing_situations = ref Situations.When_needed
  let parens_situations_cmd =
    let open Arg in
    let doc =
      Format.asprintf
        "when to add parentheses around match expressions. %a"
        (one_of Situations.print) Situations.all
    in
    let info = formatting_info ~doc ["match-parenthezing"] in
    parenthesing_situations := value & opt Situations.t When_needed info
end

module Cases = struct
  let body_indent = ref 2
  let body_indent_cmd =
    let open Arg in
    let info =
      formatting_info ~doc:"indentation of match/function cases body"
        [ "cases-body-indent" ]
    in
    body_indent := value & opt int 2 info

  let body_on_separate_line = ref Situations.When_needed
  let body_on_separate_line_cmd =
    let open Arg in
    let doc =
      Format.asprintf
        "when to put the body on a separate line. %a"
        (one_of Situations.print) Situations.all
    in
    let info = formatting_info ~doc [ "cases-body-on-separate-line" ] in
    body_on_separate_line := value & opt Situations.t When_needed info
end

module Applications = struct
  let layout = ref Smartly_wrappable.Wrap
  let layout_cmd =
    let open Arg in
    let doc =
      Format.asprintf
        "formatting of function applications. %a"
        (one_of Smartly_wrappable.print) Smartly_wrappable.all
    in
    let info = formatting_info ~doc ["fun-app"] in
    layout := value & opt Smartly_wrappable.t Wrap info
end
