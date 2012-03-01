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

(* $Id: arch.ml 7784 2007-01-01 13:07:35Z xleroy $ *)

(* Machine-specific command-line options *)

let pic_code = ref true

let command_line_options =
  [ "-fPIC", Arg.Set pic_code,
      " Generate position-independent machine code (default)";
    "-fno-PIC", Arg.Clear pic_code,
      " Generate position-dependent machine code" ]

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

(* Instruction selection *)

let word_addressed = false
