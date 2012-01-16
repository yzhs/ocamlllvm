open Llvm_mach

type label = string

type cast = Zext | Trunc | Bitcast | Inttoptr | Ptrtoint

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: ssa_reg array;
    res: ssa_reg;
    dbg: Debuginfo.t }

and instruction_desc =
    Lend
  | Lcast of cast
  | Lcomp of comp 
  | Lop of binop
  | Lfptosi | Lsitofp
  | Lalloca | Lload | Lstore | Lgetelemptr
  | Lcall of ssa_reg | Lextcall of ssa_reg
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of label * label 
  | Lswitch of label * label array
  | Lunreachable
  | Lcomment of string

type fundecl =
  { fun_name: string;
    fun_args: ssa_reg list;
    fun_body: instruction }

val end_instr : instruction

val instr_cons :
  instruction_desc -> ssa_reg array -> ssa_reg -> instruction -> instruction

val cons_instr :
  instruction_desc -> instruction -> instruction

val fundecl: Llvm_mach.fundecl -> fundecl

val string_of_cast : cast -> string
