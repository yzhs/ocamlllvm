exception Llvm_error of string

type 'a error = Just of 'a | Error of string

type llvm_type =
    Integer of int (* bitwidth *)
  | Double
  | Address of llvm_type
  | Void
  | Any
  | Function of llvm_type * llvm_type list (* return type, argument types *)

type cast = Zext | Trunc | Bitcast | Inttoptr | Ptrtoint | Sitofp | Fptosi

type binop =
    Op_addi | Op_subi | Op_muli | Op_divi | Op_modi
  | Op_and | Op_or | Op_xor | Op_lsl | Op_lsr | Op_asr
  | Op_addf | Op_subf | Op_mulf | Op_divf

type llvm_instr =
    Lvar of string * llvm_type (* name, type *)
  | Lbinop of binop * llvm_type * llvm_instr * llvm_instr (* op, typ, left, right *)
  | Lcomp of string * llvm_type * llvm_instr * llvm_instr (* op, typ, left, right *)
  | Lalloca of string * llvm_type (*name, typ*)
  | Lload of llvm_instr (* address *)
  | Lstore of llvm_instr * llvm_instr (* value, address *)
  | Lcast of cast * llvm_instr * llvm_type * llvm_type
  | Lgetelementptr of llvm_instr * llvm_instr (* address, offset *)
  | Lcall of llvm_type * llvm_instr * llvm_instr list (* return type, name, arguments *)
  | Lccall of llvm_type * llvm_instr * llvm_instr list (* return type, name, arguments; using c calling convention *)
  | Lconst of string * llvm_type (* literal, type *)
  | Llabel of string (* name *)
  | Lbr of string (* label *)
  | Lbr_cond of llvm_instr * string * string (* condition, then label, else label *)
  | Lswitch of string * llvm_instr * int array * llvm_instr * llvm_instr array * llvm_type (* indexes, blocks *)
  | Lreturn of llvm_instr * llvm_type (* value, type *)
  | Lseq of llvm_instr * llvm_instr (* value, type *)
  | Lcaml_raise_exn of llvm_instr (* argument *)
  | Lcaml_catch_exn of string * llvm_instr * llvm_instr (* ident, what to do, where to store result *)
  | Lcaml_alloc of int (* length *)
  | Ldefine of string * (string * llvm_type) list * llvm_instr (* name, arguments, body *)
  | Lnothing
  | Lunreachable
  | Lcomment of string


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

val string_of_binop : binop -> string
val string_of_cast : cast -> string

(* Turn an llvm_type into the string used by LLVM to represent that type. *)
val string_of_type : llvm_type -> string
(* For a pointer return what type its target has. *)
val deref : llvm_type -> llvm_type

(* Indicates whether the given [llvm_instr] returns a value in an SSA-register. *)
val has_type : llvm_instr -> bool
(* Returns the type of result of the given instruction. *)
val typeof : llvm_instr -> llvm_type

(* Turn two [llvm_instr] into a single one which does the first and then the
 * second *)
val (@@) : llvm_instr -> llvm_instr -> llvm_instr

(* The usual monad functions for ['a error]. *)
val return : 'a -> 'a error
val fail : string -> 'a error
val (>>=) : 'a error -> ('a -> 'b error) -> 'b error
val (++) : 'a error -> 'b error -> ('a * 'b) error
