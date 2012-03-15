open Aux
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


let rec dummy_instr =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = Nothing;
    typ = Void;
    dbg = Debuginfo.none }

let end_instr () =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = Nothing;
    typ = Void;
    dbg = Debuginfo.none }

let instr_cons d a r t n =
  { desc = d; next = n; arg = a; res = r; typ = t; dbg = Debuginfo.none }

let instr_cons_debug d a r t dbg n =
  { desc = d; next = n; arg = a; res = r; typ = t; dbg = dbg }

let string_of_binop = function
  | Op_addi -> "add"
  | Op_subi -> "sub"
  | Op_muli -> "mul"
  | Op_divi -> "sdiv"
  | Op_modi -> "srem"
  | Op_and  -> "and"
  | Op_or   -> "or"
  | Op_xor  -> "xor"
  | Op_lsl  -> "shl"
  | Op_lsr  -> "lshr"
  | Op_asr  -> "ashr"
  | Op_addf -> "fadd"
  | Op_subf -> "fsub"
  | Op_mulf -> "fmul"
  | Op_divf -> "fdiv"

let string_of_comp typ =
  match typ with
  | Double -> begin
      function
      | Comp_eq -> "fcmp oeq"
      | Comp_ne -> "fcmp une" (* this one has to be unordered apparently *)
      | Comp_lt -> "fcmp olt"
      | Comp_le -> "fcmp ole"
      | Comp_gt -> "fcmp ogt"
      | Comp_ge -> "fcmp oge"
    end
  | Integer _ -> begin
      function
      | Comp_eq -> "icmp  eq"
      | Comp_ne -> "icmp  ne"
      | Comp_lt -> "icmp slt"
      | Comp_le -> "icmp sle"
      | Comp_gt -> "icmp sgt"
      | Comp_ge -> "icmp sge"
    end
  | Address _ -> begin
      function
      | Comp_eq -> "icmp  eq"
      | Comp_ne -> "icmp  ne"
      | Comp_lt -> "icmp ult"
      | Comp_le -> "icmp ule"
      | Comp_gt -> "icmp ugt"
      | Comp_ge -> "icmp uge"
    end
  | _ -> error "no comparison operations are defined for this type"
