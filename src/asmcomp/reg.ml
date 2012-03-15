open Arch

exception Cast_error of string

(* Representation of LLVMs types *)
type llvm_type =
    Integer of int (* bitwidth *)
  | Double
  | Address of llvm_type
  | Jump_buffer (* The type of a buffer used by setjmp/longjmp *)
  | Void
  | Any (* Used during type inference to signal that any type would be ok *)
  | Function of llvm_type * llvm_type list (* return type, argument types *)

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

let int_type = Integer (8 * size_int)
let addr_type = Address int_type
let float_sized_int = Integer (8 * size_float)
let byte = Integer 8
let bit = Integer 1
let function_type ret args = Address(Function(ret, args))

let is_addr = function Address _ -> true | _ -> false
let is_float = function Double -> true | _ -> false
let is_int = function Integer _ -> true | _ -> false

let is_function = function
  | Function(_,_) | Address(Function(_,_)) -> true
  | _ -> false

let ret_type = function
  | Address(Function(ret, _)) -> ret
  | _ -> raise (Cast_error "not a function")

let arg_types = function
  | Address(Function(_, args)) -> args
  | _ -> raise (Cast_error "not a function")





type register = Const of string * llvm_type | Reg of string * llvm_type | Nothing

let reg_counter = ref 0
let reset_counter () = reg_counter := 0

let new_reg name typ =
  reg_counter := !reg_counter + 1;
  Reg(name ^ "." ^ string_of_int !reg_counter, typ)


(* Print an expression in the intermediate format using a syntax inspired by
 * S-expressions *)
let reg_name = function
    Const(value, _) -> value
  | Reg(value, _) -> "%" ^ value
  | Nothing -> "no_register"

let typeof = function
    Const(_, typ) -> typ
  | Reg(_, typ) -> typ
  | Nothing -> Void

let string_of_reg = function
    Const(value, typ) -> string_of_type typ ^ " " ^ value
  | Reg(value, typ) -> string_of_type typ ^ " %" ^ value
  | Nothing -> "void no_register"
