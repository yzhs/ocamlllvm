(* Emit LLVM assembly for the given C-- function. *)
val compile_fundecl : Cmm.fundecl -> unit

(* Go through all the phrases to find out the types of all functions in the
 * current module. *)
val read_function : Cmm.phrase -> unit
