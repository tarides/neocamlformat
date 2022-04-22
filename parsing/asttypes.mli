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

(** Auxiliary AST types used by parsetree and typedtree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

(* Order matters, used in polymorphic comparison *)
type private_flag = Private | Public

type mutable_flag = Immutable | Mutable of Location.t

type virtual_flag = Virtual | Concrete

type private_virtual =
  | PV_none
  | PV_private
  | PV_virtual
  | PV_priv_virt
  | PV_virt_priv

type mutable_virtual =
  | MV_none
  | MV_mutable of Location.t
  | MV_virtual
  | MV_mut_virt
  | MV_virt_mut

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type label = string

type and_or_with = And | With

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
}

type label_info = {
  name: string loc;
  extra_info: [ `Single_token | `Previous_token of Location.t ];
}

type arg_label =
    Nolabel
  | Labelled of label_info (*  label:T -> ... *)
  | Optional of label_info (* ?label:T -> ... *)

type variance_and_inj = string loc list

(* For Pexp_dotop_[gs]et *)
type paren_kind = Paren | Brace | Bracket
