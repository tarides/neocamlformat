open Cmdliner

let print_all ?sep print =
  let open Format in
  let p sep =
    pp_print_list ~pp_sep:(fun ppf () -> pp_print_string ppf sep) print
  in
  match sep with
  | Some sep -> p sep
  | None ->
    fun ppf ->
      Format.fprintf ppf "{%a}" (p "|")

let accepted_values print ppf values =
  Format.fprintf ppf "accepted values: %a" (print_all ~sep:", " print) values

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

module Always_or_needed = struct
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

module Compact_or_multiline = struct
  type t = Compact | Multi | Compact_under_app
  let all = [ Compact; Multi; Compact_under_app ]

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Compact -> "compact"
       | Multi -> "multiline"
       | Compact_under_app -> "compact-under-app")

  let parse = function
    | "compact" -> Ok Compact
    | "multiline" -> Ok Multi
    | "compact-under-app" -> Ok Compact_under_app
    | s ->
      let msg = Format.asprintf "%a, got %S" (accepted_values print) all s in
      Error (`Msg msg)

  let t = Arg.conv (parse, print)
end

let (:=) ref term =
  Term.(const (fun x -> ref := x) $ term)

let docs = "FORMATTING OPTIONS"

let mk_info print all =
  let docv = Format.asprintf "%a" (print_all print) all in
  Arg.info ~docs ~docv

module Record = struct
  open Wrappable

  let expression = ref Fit_or_vertical
  let expression_cmd =
    let open Arg in
    let doc = "formatting of record expressions" in
    let info = mk_info print all ~doc ["record-expr"] in
    expression := value & opt t Fit_or_vertical info

  let pattern = ref Wrap
  let pattern_cmd =
    let open Arg in
    let doc = "formatting of record patterns" in
    let info = mk_info print all ~doc ["record-patt"] in
    pattern := value & opt t Wrap info
end

module Match = struct
  module P = Parenthesing

  let parens_style = ref P.Begin_end
  let parens_style_cmd =
    let open Arg in
    let doc = "style of parenthesing to use around match expressions" in
    let info =
      mk_info P.print P.all ~doc
        ["match-parens-style" ;"match-parenthezing-style"]
    in
    parens_style := value & opt P.t Begin_end info

  let parenthesing_situations = ref Always_or_needed.When_needed
  let parens_situations_cmd =
    let open Arg in
    let doc = "when to add parentheses around match expressions" in
    let info = mk_info Always_or_needed.print Always_or_needed.all ~doc ["match-parenthezing"] in
    parenthesing_situations := value & opt Always_or_needed.t When_needed info

  let compact = ref Compact_or_multiline.Compact_under_app
  let compact_cmd =
    let open Compact_or_multiline in
    let open Arg in
    let doc = "whether or not to print match expressions on a single line if \
               it fits. This also applies to functions and try-withs." in
    let info = mk_info print all ~doc ["match-layout"] in
    compact := value & opt t Compact_under_app info
end

module Cases = struct
  let body_indent = ref 2
  let body_indent_cmd =
    let open Arg in
    let info =
      info ~docs ~doc:"indentation of match/function cases body"
        [ "cases-body-indent" ]
    in
    body_indent := value & opt int 2 info

  let body_on_separate_line = ref Always_or_needed.When_needed
  let body_on_separate_line_cmd =
    let open Arg in
    let open Always_or_needed in
    let doc = "when to put the body on a separate line" in
    let info = mk_info print all ~doc [ "cases-body-on-separate-line" ] in
    body_on_separate_line := value & opt t When_needed info
end

module Applications = struct
  open Smartly_wrappable

  let layout = ref Wrap
  let layout_cmd =
    let open Arg in
    let doc = "formatting of function applications" in
    let info = mk_info print all ~doc ["fun-app"] in
    layout := value & opt t Wrap info
end

module Sequences = struct
  let compact = ref Compact_or_multiline.Compact_under_app
  let compact_cmd =
    let open Compact_or_multiline in
    let open Arg in
    let doc =
      "whether or not to print sequences on a single line if they fit"
    in
    let info = mk_info print all ~doc ["sequences-layout"] in
    compact := value & opt t Compact_under_app info
end
