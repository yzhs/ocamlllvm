exception Cast_error of string

type llvm_type =
    Integer of int (* bitwidth *)
  | Double
  | Address of llvm_type
  | Jump_buffer
  | Void
  | Any
  | Function of llvm_type * llvm_type list (* return type, argument types *)

type register = Const of string * llvm_type | Reg of string * llvm_type | Nothing


val string_of_type: llvm_type -> string

val deref: llvm_type -> llvm_type

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
(* Create a function type given the return and argument types *)
val function_type : llvm_type -> llvm_type list -> llvm_type

val is_addr : llvm_type -> bool
val is_float : llvm_type -> bool
val is_int : llvm_type -> bool
val is_function : llvm_type -> bool

val ret_type : llvm_type -> llvm_type
val arg_types : llvm_type -> llvm_type list

(* Turn an llvm_type into the string used by LLVM to represent that type. *)
val string_of_type : llvm_type -> string

(* For a pointer return what type its target has. *)
val deref : llvm_type -> llvm_type


val reset_counter: unit -> unit
val new_reg: string -> llvm_type -> register

(* Returns the type of result of the given instruction. *)
val typeof: register -> llvm_type
val reg_name: register -> string
val string_of_reg: register -> string 
