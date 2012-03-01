(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: proc_nt.ml 9547 2010-01-22 12:48:24Z doligez $ *)

(* Description of the AMD64 processor with Win64 conventions *)

(* Calling the assembler *)

let assemble_file infile outfile =
  Ccomp.command (Config.asm ^
                 Filename.quote outfile ^ " " ^
                 Filename.quote infile ^ "> NUL")
