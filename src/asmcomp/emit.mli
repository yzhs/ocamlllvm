(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Generation of assembly code *)

(* Emit data declarations using LLVM's module level assembly *)
val data: Cmm.data_item list -> unit

(* Print the header *)
val begin_assembly : unit -> unit
(* Emit the function and variable declarations *)
val end_assembly : unit -> unit
