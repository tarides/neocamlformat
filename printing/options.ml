module Choice : sig
  type 'a t

  type err = Choice_already_made

  val mint : string -> default:'a -> 'a t

  val get : 'a t -> 'a
(*   val run : 'a t list -> 'a option Term.t -> unit Term.t *)
  val setopts : 'a t list -> 'a option -> (unit, [ `Msg of string ]) result
end = struct
  type 'a t = {
    mutable user: 'a option;
    name: string;
    default: 'a;
  }

  let mint name ~default = { name; default; user = None }

  type err = Choice_already_made

  let get { user; default; _ } = Option.value user ~default

  let set t choice =
    if Option.is_some t.user then
      Error (`Msg (Printf.sprintf "overlapping flags for %s" t.name))
    else
      Ok (t.user <- Some choice)

  let setopt t = function
    | None -> Ok ()
    | Some x -> set t x

  let rec iter_m ~f = function
    | [] -> Ok ()
    | x :: xs -> Result.bind (f x) (fun () -> iter_m ~f xs)

  let setopts ts choice = iter_m ~f:(fun t -> setopt t choice) ts
end

module Wrappable = struct
  type t = Fit_or_vertical | Wrap

  let parse = function
    | "fit-or-vertical" -> Ok (Some Fit_or_vertical)
    | "wrap" -> Ok (Some Wrap)
    | s ->
      let msg = Printf.sprintf "accepted fit-or-vertical | wrap, got %S" s in
      Error (`Msg msg)

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Some Fit_or_vertical -> "fit-or-vertical"
       | Some Wrap -> "docked"
       | _ -> "default")

  let t = Cmdliner.Arg.conv (parse, print)
end

module Parenthesing = struct
  type t = Parens | Begin_end

  let parse = function
    | "parens"
    | "parentheses" -> Ok (Some Parens)
    | "begin-end"
    | "begin" -> Ok (Some Begin_end)
    | s ->
      let msg =
        Printf.sprintf
          "accepted parens | parentheses | begin | begin-end, got %S" s
      in
      Error (`Msg msg)

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Some Parens -> "parentheses"
       | Some Begin_end -> "begin-end"
       | _ -> "default")

  let t = Cmdliner.Arg.conv (parse, print)
end

module Situations = struct
  type t = When_needed | Always

  let parse = function
    | "always" -> Ok (Some Always)
    | "when-needed" -> Ok (Some When_needed)
    | s ->
      let msg =
        Printf.sprintf
          "accepted always | when-needed, got %S" s
      in
      Error (`Msg msg)

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Some Always -> "always"
       | Some When_needed -> "when_needed"
       | _ -> "default")

  let t = Cmdliner.Arg.conv (parse, print)
end

module Record = struct
  let expression_choice =
    Choice.mint "record expressions" ~default:Wrappable.Fit_or_vertical

  let pattern_choice =
    Choice.mint "record patterns" ~default:Wrappable.Wrap

  open Cmdliner

  let expression_arg =
    let open Arg in
    let info = info ~doc:"formatting of record expressions" ["record-expr"] in
    value & opt Wrappable.t None info

  let pattern_arg =
    let open Arg in
    let info = info ~doc:"formatting of record patterns" ["record-patt"] in
    value & opt Wrappable.t None info

  let all_arg =
    let open Arg in
    let info = info ~doc:"formatting of records (in all contexts)" ["record"] in
    value & opt Wrappable.t None info
end

module Match = struct
  let parens_style_choice =
    Choice.mint "match parenthesing style" ~default:Parenthesing.Begin_end

  let parenthesing_situations_choice =
    Choice.mint "match parenthesing situations" ~default:Situations.When_needed

  open Cmdliner

  let parens_style_arg =
    let open Arg in
    let info =
      info ~doc:"style of parenthesing to use around match expressions"
        ["match-parens-style" ;"match-parenthezing-style"]
    in
    value & opt Parenthesing.t None info

  let parens_situations_arg =
    let open Arg in
    let info =
      info ~doc:"when to add parentheses around match expressions"
        ["match-parenthezing"]
    in
    value & opt Situations.t None info
end
