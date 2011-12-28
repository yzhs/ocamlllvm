open Llvm_types

(* Add the name of a global variable to the list of global variables. *)
val add_const : string -> unit
(* Add a function declaration. *)
val add_function : (llvm_type * string * string * (llvm_instr list)) -> unit

(* Functions used in the current module (can be omitted when printing function
 * decarations). *)
val local_functions : (string * llvm_type list) list ref

(* Print the header *)
val begin_assembly : unit -> unit
(* Emit the function and variable declarations *)
val end_assembly : unit -> unit

(* Emit an llvm instruction and return [Just reg] where [reg] is the name of the
 * SSA register containing the result of that instruction or [Error s] where [s]
 * is a string containing an error message *)
val emit_llvm : llvm_instr -> string error

(* Print the internal representation of an LLVM instruction in a notation
 * inspired by S-expressions *)
val to_string : llvm_instr -> string

(* Emit data declarations using LLVM's module level assembly *)
val data : Cmm.data_item list -> unit

(* Assemble the file given by the first argument, using the second and third
 * arguments as temporary files (for LLVM assembly) producing the actual result
 * in the file given in the second argument. The result is 0 if everything
 * worked fine and the return value of the first command that went wrong. *)
val assemble_file : string -> string -> string -> string -> int

(* Print a debugging message to stdout *)
val print_debug : string -> unit

(* The name of the internal calling convention. *)
val calling_conv : string
