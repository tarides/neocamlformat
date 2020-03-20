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

(** Entry points in the parser

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val implementation : Lexing.lexbuf -> Source_tree.structure
val interface : Lexing.lexbuf -> Source_tree.signature
val toplevel_phrase : Lexing.lexbuf -> Source_tree.toplevel_phrase
val use_file : Lexing.lexbuf -> Source_tree.toplevel_phrase list
val core_type : Lexing.lexbuf -> Source_tree.core_type
val expression : Lexing.lexbuf -> Source_tree.expression
val pattern : Lexing.lexbuf -> Source_tree.pattern
