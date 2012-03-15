open Reg
open Mach

type label = string

type cast = Zext | Sext | Trunc | Bitcast | Inttoptr | Ptrtoint

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: register array;
    res: register;
    dbg: Debuginfo.t }

and instruction_desc =
    Lend
  | Lcast of cast
  | Lcomp of comp 
  | Lop of binop
  | Lfptosi | Lsitofp
  | Lalloca | Lload | Lstore | Lgetelemptr
  | Lcall of register | Lextcall of register
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of label * label 
  | Lswitch of label * label array
  | Lunreachable
  | Lcomment of string

type fundecl =
  { fun_name: string;
    fun_args: register list;
    fun_body: instruction }

val end_instr : instruction

val instr_cons :
  instruction_desc -> register array -> register -> instruction -> instruction

val cons_instr :
  instruction_desc -> instruction -> instruction

val fundecl: Mach.fundecl -> fundecl

val string_of_cast : cast -> string
