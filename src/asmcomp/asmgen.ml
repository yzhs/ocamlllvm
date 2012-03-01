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

(* From lambda to assembly code *)

open Format
open Config
open Clflags
open Misc
open Cmm
open Reg

type error = Assembler_error of string

exception Error of error

let (++) x f = f x

let read_function phrase =
  match phrase with
  | Cfunction fd_cmm ->
      let name = fd_cmm.fun_name in
      let args = List.map (fun _ -> addr_type) fd_cmm.fun_args in
      Emit_common.local_functions := (Aux.translate_symbol name, args) :: !Emit_common.local_functions
  | Cdata _ -> ()

let dump print x = if !debug then print x; x

let compile_fundecl fd_cmm =
  fd_cmm
  ++ Selectgen.fundecl
(*  ++ dump (fun fn -> print_endline (Aux.to_string fn.body)) *)
  ++ Linearize.fundecl
  ++ Emit_common.fundecl

let begin_assembly = Emit.begin_assembly

let end_assembly = Emit.end_assembly

let run_assembler asm infile outfile =
  Ccomp.command (asm ^ " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let assemble_file temp1 temp2 infile outfile =
  let res = run_assembler Config.opt infile temp1 in 
  if res <> 0 then res else
  let res = run_assembler Config.llc temp1 temp2 in 
  if res <> 0 then res else
    Proc.assemble_file temp2 outfile

let compile_phrase ppf p =
  if !dump_cmm then fprintf ppf "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl fd
  | Cdata dl -> Emit.data dl


(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ppf f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ppf ph
       | _ -> ())
    (Cmmgen.generic_functions true [Compilenv.current_unit_infos ()])

let compile_implementation ?toplevel prefixname ppf (size, lam) =
  let asmfile =
    if !keep_asm_file
    then prefixname ^ ext_llvm
    else Filename.temp_file "camlasm" ext_llvm in
  let oc = open_out asmfile in
  begin try
    Emitaux.output_channel := oc;
    Emit.begin_assembly();
    Closure.intro size lam
    ++ Cmmgen.compunit size
    ++ List.map (fun x -> read_function x; x)
    ++ List.iter (compile_phrase ppf) ++ (fun () -> ());
    (match toplevel with None -> () | Some f -> compile_genfuns ppf f);

    (* We add explicit references to external primitive symbols.  This
       is to ensure that the object files that define these symbols,
       when part of a C library, won't be discarded by the linker.
       This is important if a module that uses such a symbol is later
       dynlinked. *)

    compile_phrase ppf
      (Cmmgen.reference_symbols
         (List.filter (fun s -> s <> "" && s.[0] <> '%')
            (List.map Primitive.native_name !Translmod.primitive_declarations))
      );

    Emit.end_assembly();
    close_out oc
  with x ->
    close_out oc;
    if !keep_asm_file then () else remove_file asmfile;
    raise x
  end;
  let temp1 =
    if !Clflags.keep_asm_file then prefixname ^ ".opt" ^ ext_llvm
    else Filename.temp_dir_name ^ "/" ^ Filename.basename prefixname ^ ext_llvm
  in
  let temp2 =
    if !Clflags.keep_asm_file then prefixname ^ ext_asm
    else Filename.temp_dir_name ^ "/" ^ Filename.basename prefixname ^ ext_asm
  in
  if assemble_file temp1 temp2 asmfile (prefixname ^ ext_obj) <> 0
    then raise(Error(Assembler_error asmfile));
  if !keep_asm_file then ()
  else begin
    remove_file asmfile;
    remove_file temp1;
    remove_file temp2
  end

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
      fprintf ppf "Assembler error, input left in file %s" file
