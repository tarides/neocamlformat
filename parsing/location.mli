(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {1 Source code locations (ranges of positions), used in parsetree}

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Format

type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
}

val ends_before : t -> t -> bool

(** Note on the use of Lexing.position in this module.
   If [pos_fname = ""], then use [!input_name] instead.
   If [pos_lnum = -1], then [pos_bol = 0]. Use [pos_cnum] and
     re-parse the file to get the line and character numbers.
   Else all fields are correct.
*)

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)

val in_file : string -> t
(** Return an empty ghost range located in a given file. *)

val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)

val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t

(** [rhs_loc n] returns the location of the symbol at position [n], starting
  at 1, in the current parser rule. *)
val rhs_loc: int -> t

val rhs_interval: int -> int -> t

val get_pos_info: Lexing.position -> string * int * int
(** file, line, char *)

type 'a loc = {
  txt : 'a;
  loc : t;
}

val mknoloc : 'a -> 'a loc
val mkloc : 'a -> t -> 'a loc


(** {1 Input info} *)

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

(* This is used for reporting errors coming from the toplevel.

   When running a toplevel session (i.e. when [!input_name] is "//toplevel//"),
   [!input_phrase_buffer] should be [Some buf] where [buf] contains the last
   toplevel phrase. *)
val input_phrase_buffer: Buffer.t option ref


(** {1 Toplevel-specific functions} *)

val echo_eof: unit -> unit
val reset: unit -> unit


(** {1 Printing locations} *)

val print_loc: formatter -> t -> unit
val print_locs: formatter -> t list -> unit


(** {1 Reporting errors and warnings} *)

(** {2 The type of reports and report printers} *)

type msg = (Format.formatter -> unit) loc

val msg: ?loc:t -> ('a, Format.formatter, unit, msg) format4 -> 'a

type report_kind =
  | Report_error
  | Report_warning of string
  | Report_warning_as_error of string
  | Report_alert of string
  | Report_alert_as_error of string

type report = {
  kind : report_kind;
  main : msg;
  sub : msg list;
}

type report_printer = {
  (* The entry point *)
  pp : report_printer ->
    Format.formatter -> report -> unit;

  pp_report_kind : report_printer -> report ->
    Format.formatter -> report_kind -> unit;
  pp_main_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_main_txt : report_printer -> report ->
    Format.formatter -> (Format.formatter -> unit) -> unit;
  pp_submsgs : report_printer -> report ->
    Format.formatter -> msg list -> unit;
  pp_submsg : report_printer -> report ->
    Format.formatter -> msg -> unit;
  pp_submsg_loc : report_printer -> report ->
    Format.formatter -> t -> unit;
  pp_submsg_txt : report_printer -> report ->
    Format.formatter -> (Format.formatter -> unit) -> unit;
}
(** A printer for [report]s, defined using open-recursion.
    The goal is to make it easy to define new printers by re-using code from
    existing ones.
*)

(** {2 Report printers used in the compiler} *)

val batch_mode_printer: report_printer

(** {2 Printing a [report]} *)

val print_report: formatter -> report -> unit
(** Display an error or warning report. *)

val report_printer: (unit -> report_printer) ref
(** Hook for redefining the printer of reports.

    The hook is a [unit -> report_printer] and not simply a [report_printer]:
    this is useful so that it can detect the type of the output (a file, a
    terminal, ...) and select a printer accordingly. *)

val default_report_printer: unit -> report_printer
(** Original report printer for use in hooks. *)


(** {1 Reporting errors} *)

type error = report
(** An [error] is a [report] which [report_kind] must be [Report_error]. *)

val error: ?loc:t -> ?sub:msg list -> string -> error

val errorf: ?loc:t -> ?sub:msg list ->
  ('a, Format.formatter, unit, error) format4 -> 'a

val error_of_printer: ?loc:t -> ?sub:msg list ->
  (formatter -> 'a -> unit) -> 'a -> error

val error_of_printer_file: (formatter -> 'a -> unit) -> 'a -> error


(** {1 Automatically reporting errors for raised exceptions} *)

val register_error_of_exn: (exn -> error option) -> unit
(** Each compiler module which defines a custom type of exception
    which can surface as a user-visible error should register
    a "printer" for this exception using [register_error_of_exn].
    The result of the printer is an [error] value containing
    a location, a message, and optionally sub-messages (each of them
    being located as well). *)

val error_of_exn: exn -> [ `Ok of error | `Already_displayed ] option

exception Error of error
(** Raising [Error e] signals an error [e]; the exception will be caught and the
   error will be printed. *)

exception Already_displayed_error
(** Raising [Already_displayed_error] signals an error which has already been
   printed. The exception will be caught, but nothing will be printed *)

val raise_errorf: ?loc:t -> ?sub:msg list ->
  ('a, Format.formatter, unit, 'b) format4 -> 'a

val report_exception: formatter -> exn -> unit
(** Reraise the exception if it is unknown. *)

val start_point : t -> t
val end_point : t -> t
