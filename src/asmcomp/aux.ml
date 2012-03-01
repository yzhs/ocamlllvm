(*
val debug : bool ref
(* Print a debugging message to stdout *)
val print_debug : string -> unit

val (++) : 'a -> ('a -> 'b) -> 'b

val reg_name : reg -> string

val string_of_reg : reg -> string

(* Print the internal representation of an LLVM instruction in a notation
 * inspired by S-expressions *)
val to_string : Llvm_mach.instruction -> string
 *)

exception Llvm_error of string

(* Raise an Llvm_error with the string given as an argument. *)
let error s = raise (Llvm_error s)

let debug = ref false

let print_debug str = if !debug then print_endline str

let translate_symbol s =
  let result = ref "" in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      |'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        result := !result ^ Printf.sprintf "%c" c
      | _ -> result := !result ^ Printf.sprintf "$%02x" (Char.code c)
  done;
  !result
