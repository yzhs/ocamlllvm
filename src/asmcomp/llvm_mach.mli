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

type ssa_reg = Const of string * llvm_type | Reg of string * llvm_type | Nothing

type binop =
    Op_addi | Op_subi | Op_muli | Op_divi | Op_modi
  | Op_and | Op_or | Op_xor | Op_lsl | Op_lsr | Op_asr
  | Op_addf | Op_subf | Op_mulf | Op_divf

type comp = Comp_eq | Comp_ne | Comp_lt | Comp_le | Comp_gt | Comp_ge

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

val dummy_instr : instruction

val end_instr : unit -> instruction

val instr_cons
  : instruction_desc -> ssa_reg array -> ssa_reg -> llvm_type -> instruction -> instruction

val instr_cons_debug
  : instruction_desc -> ssa_reg array -> ssa_reg -> llvm_type -> Debuginfo.t -> instruction -> instruction


(* Raise an Llvm_error with the string given as an argument. *)
val error : string -> 'a

(* The length of an address in bits *)
val size_addr : int
(* The length of an integer in bits *)
val size_int : int
(* The length of a floating point number *)
val size_float : int

(* integer type with [size_int] bits *)
val int_type : llvm_type
(* pointer to [int_type] *)
val addr_type : llvm_type
(* an integer with [size_float] bits *)
val float_sized_int : llvm_type
(* 8 bit long integer *)
val byte : llvm_type
(* 1 bit *)
val bit : llvm_type

val is_addr : llvm_type -> bool
val is_float : llvm_type -> bool
val is_int : llvm_type -> bool

val string_of_binop : binop -> string

(* Turn an llvm_type into the string used by LLVM to represent that type. *)
val string_of_type : llvm_type -> string
(* For a pointer return what type its target has. *)
val deref : llvm_type -> llvm_type

(* Returns the type of result of the given instruction. *)
val typeof : ssa_reg -> llvm_type

val reset_counter : unit -> unit
val register : string -> llvm_type -> ssa_reg

val string_of_comp : llvm_type -> comp -> string
