exception Llvm_error of string
exception Cast_error of string

type llvm_type =
    Integer of int (* bitwidth *)
  | Double
  | Address of llvm_type
  | Jump_buffer
  | Void
  | Any
  | Function of llvm_type * llvm_type list (* return type, argument types *)

type binop =
    Op_addi | Op_subi | Op_muli | Op_divi | Op_modi
  | Op_and | Op_or | Op_xor | Op_lsl | Op_lsr | Op_asr
  | Op_addf | Op_subf | Op_mulf | Op_divf

type comp = Comp_eq | Comp_ne | Comp_lt | Comp_le | Comp_gt | Comp_ge

type ssa_reg = Const of string * llvm_type | Reg of string * llvm_type | Nothing

type instruction =
  { desc: instruction_desc;
    next: instruction;
    arg: ssa_reg array;
    res: ssa_reg;
    typ: llvm_type;
    dbg: Debuginfo.t }

and instruction_desc =
    Iend
  | Ibinop of binop
  | Icomp of comp
  | Ialloca | Iload | Istore
  | Isitofp | Ifptosi
  | Igetelementptr
  | Icall of ssa_reg | Iextcall of ssa_reg
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


let error s = raise (Llvm_error s)

let size_addr = Arch.size_addr
let size_int = Arch.size_int
let size_float = Arch.size_float

let int_type = Integer (8 * size_int)
let addr_type = Address int_type
let float_sized_int = Integer (8 * size_float)
let byte = Integer 8
let bit = Integer 1

let is_addr = function Address _ -> true | _ -> false
let is_float = function Double -> true | _ -> false
let is_int = function Integer _ -> true | _ -> false

let rec string_of_type = function
  | Integer i -> "i" ^ string_of_int i
  | Double -> "double"
  | Address typ -> string_of_type typ ^ "*"
  | Jump_buffer -> "%jump_buf_t"
  | Void -> "void"
  | Function(ret, args) -> string_of_type ret ^ " (" ^ String.concat ", " (List.map string_of_type args) ^ ")"
  | Any -> (*error "unable to infer type"*) "Any"

let deref typ = match typ with
  | Address typ -> typ
  | _ -> raise (Cast_error ("trying to dereference non-pointer type " ^ string_of_type typ))

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

let typeof = function
    Const(_, typ) -> typ
  | Reg(_, typ) -> typ
  | Nothing -> Void

let reg_counter = ref 0
let reset_counter () = reg_counter := 0
let new_reg () = reg_counter := !reg_counter + 1; "." ^ string_of_int !reg_counter 

let register name typ = Reg(name ^ new_reg(), typ)

let string_of_comp typ =
  match typ with
  | Double -> begin
      function
      | Comp_eq -> "fcmp oeq"
      | Comp_ne -> "fcmp one"
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
