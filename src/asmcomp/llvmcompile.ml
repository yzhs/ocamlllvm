open Cmm
open Emitaux
open Llvmemit
open Llvm_aux
open Llvm_mach

let read_function phrase =
  match phrase with
  | Cfunction fd_cmm ->
      let name = fd_cmm.fun_name in
      let args = List.map (fun _ -> addr_type) fd_cmm.fun_args in
      local_functions := (Llvm_selectgen.translate_symbol name, args) :: !local_functions
  | Cdata _ -> ()

let dump print x = if !debug then print x; x

let compile_fundecl fd_cmm =
  fd_cmm
  ++ Llvm_selectgen.fundecl
(*  ++ dump (fun fn -> print_endline (Llvm_aux.to_string fn.body)) *)
  ++ Llvm_linearize.fundecl
  ++ Llvmemit.fundecl

let data = Llvmemit.data

let begin_assembly = Llvmemit.begin_assembly

let end_assembly = Llvmemit.end_assembly

let run_assembler asm infile outfile =
  Ccomp.command (asm ^ " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let assemble_file temp1 temp2 infile outfile =
  let res = run_assembler Config.opt infile temp1 in 
  if res <> 0 then res else
  let res = run_assembler Config.llc temp1 temp2 in 
  if res <> 0 then res else
    run_assembler Config.asm temp2 outfile
