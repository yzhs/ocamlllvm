(* Go through all the phrases to find out the types of all functions in the
 * current module. *)
val read_function : Cmm.phrase -> unit

(* Emit LLVM assembly for the given C-- function. *)
val compile_fundecl : Cmm.fundecl -> unit

(* Print the header *)
val begin_assembly : unit -> unit
(* Emit the function and variable declarations *)
val end_assembly : unit -> unit

(* Emit data declarations using LLVM's module level assembly *)
val data : Cmm.data_item list -> unit

(* Assemble the file given by the first argument, using the second and third
 * arguments as temporary files (for LLVM assembly) producing the actual result
 * in the file given in the second argument. The result is 0 if everything
 * worked fine and the return value of the first command that went wrong. *)
val assemble_file : string -> string -> string -> string -> int
