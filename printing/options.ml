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

module Dockable = struct
  type t = Fit_or_vertical | Docked

  let parse = function
    | "fit-or-vertical" -> Ok (Some Fit_or_vertical)
    | "docked" -> Ok (Some Docked)
    | s ->
      let msg = Printf.sprintf "accepted fit-or-vertical | docked, got %S" s in
      Error (`Msg msg)

  let print ppf t =
    Format.pp_print_string ppf
      (match t with
       | Some Fit_or_vertical -> "fit-or-vertical"
       | Some Docked -> "docked"
       | _ -> "default")

  let t = Cmdliner.Arg.conv (parse, print)
end

module Record = struct
  let expression_choice =
    Choice.mint "record expressions" ~default:Dockable.Fit_or_vertical

  let pattern_choice =
    Choice.mint "record patterns" ~default:Dockable.Docked

  open Cmdliner

  let expression_arg =
    let open Arg in
    let info = info ~doc:"formatting of record expressions" ["record-expr"] in
    value & opt Dockable.t None info

  let pattern_arg =
    let open Arg in
    let info = info ~doc:"formatting of record patterns" ["record-patt"] in
    value & opt Dockable.t None info

  let all_arg =
    let open Arg in
    let info = info ~doc:"formatting of records (in all contexts)" ["record"] in
    value & opt Dockable.t None info
end
