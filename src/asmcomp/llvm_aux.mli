val debug : bool ref
(* Print a debugging message to stdout *)
val print_debug : string -> unit

val (++) : 'a -> ('a -> 'b) -> 'b

val reg_to_string : Llvm_mach.ssa_reg -> string

(* Print the internal representation of an LLVM instruction in a notation
 * inspired by S-expressions *)
val to_string : Llvm_mach.instruction -> string
