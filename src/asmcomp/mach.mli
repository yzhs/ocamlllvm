open Reg

type binop =
    Op_addi | Op_subi | Op_muli | Op_divi | Op_modi
  | Op_and | Op_or | Op_xor | Op_lsl | Op_lsr | Op_asr
  | Op_addf | Op_subf | Op_mulf | Op_divf

type comp = Comp_eq | Comp_ne | Comp_lt | Comp_le | Comp_gt | Comp_ge

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: register array;
    res: register;
    typ: llvm_type;
    dbg: Debuginfo.t }

and instruction_desc =
    Iend
  | Ibinop of binop
  | Icomp of comp
  | Ialloca | Iload | Istore
  | Isitofp | Ifptosi | Izext | Isext
  | Igetelementptr
  | Icall of register | Iextcall of register * bool
  | Iifthenelse of instruction * instruction
  | Iswitch of int array * instruction array
  | Iloop of instruction
  | Iexit of int | Icatch of int * instruction * instruction
  | Ireturn
  | Iraise | Itrywith of instruction * instruction
  | Ialloc of int (* length *)
  | Iunreachable
  | Icomment of string

type fundecl =
 { name: string;
   args: (string * llvm_type) list;
   body: instruction }

val dummy_instr : instruction

val end_instr : unit -> instruction

val instr_cons
  : instruction_desc -> register array -> register -> llvm_type -> instruction -> instruction

val instr_cons_debug
  : instruction_desc -> register array -> register -> llvm_type -> Debuginfo.t -> instruction -> instruction

val string_of_binop: binop -> string
val string_of_comp: llvm_type -> comp -> string
