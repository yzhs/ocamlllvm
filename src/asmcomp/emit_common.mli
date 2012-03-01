open Reg

(* Add the name of a global variable to the list of global variables. *)
val add_const : string -> unit
(* Add a function declaration. *)
val add_function : (llvm_type * string * string * ('a list)) -> unit

(* Functions used in the current module (can be omitted when printing function
 * decarations). *)
val local_functions : (string * llvm_type list) list ref

(* Print the header *)
val begin_assembly : unit -> unit
(* Emit the function and variable declarations *)
val end_assembly : unit -> unit

(* The name of the internal calling convention. *)
val calling_conv : string

(* Emit LLVM IR for the given function. *)
val fundecl : Linearize.fundecl -> unit
