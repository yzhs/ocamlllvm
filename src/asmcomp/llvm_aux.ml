open Llvm_mach

let error s = error ("Llvm_aux: " ^ s)

let debug = ref false

let print_debug str = if !debug then print_endline str

let (++) x f = f x

(* Print an expression in the intermediate format using a syntax inspired by
 * S-expressions *)
let reg_to_string = function
    Const(value, _) -> value
  | Reg(value, _) -> "%" ^ value
  | Nothing -> "Nothing"

let rec to_string instr =
  let foo =
    let typ = instr.typ in
    let res = reg_to_string instr.res ^ " = " in
    let typ_str = string_of_type typ ^ " " ^ res in
    match instr.desc, instr.arg with
      Iend, _ -> "" (* BUG? changing this to a non-empty string causes a stack overflow *)
    | Ibinop op, [|left; right|] ->
        typ_str ^ string_of_binop op ^ " " ^ reg_to_string left ^ " " ^ reg_to_string right
    | Ibinop op, args -> error ("using binop " ^ string_of_binop op ^ " with " ^ string_of_int (Array.length args) ^ " arguments")
    | Icomp op, [|left; right|] ->
        typ_str ^ string_of_comp typ op ^ " " ^ reg_to_string left ^ " " ^ reg_to_string right
    | Icomp op, args -> error ("using comp " ^ string_of_comp typ op ^ " with " ^ string_of_int (Array.length args) ^ " arguments")
    | Ialloca, [||] -> string_of_type typ ^ "* " ^ res ^ "alloca " ^ string_of_type typ
    | Ialloca, args -> error ("using alloca with " ^ string_of_int (Array.length args) ^ " arguments")
    | Iload, [|addr|] -> typ_str ^ "load " ^ string_of_type (typeof addr) ^ " " ^ reg_to_string addr
    | Iload, args -> error ("using load with " ^ string_of_int (Array.length args) ^ " arguments")
    | Istore, [|value; addr|] ->
         "store " ^ string_of_type typ ^ " " ^ reg_to_string value ^ " " ^ string_of_type (Address typ) ^ " " ^ reg_to_string addr ^ ")"
    | Istore, args -> error ("using store with " ^ string_of_int (Array.length args) ^ " arguments")
    | Ifptosi, [|value|] -> typ_str ^ "fptosi " ^ reg_to_string value
    | Ifptosi, args -> error ("using fptosi with " ^ string_of_int (Array.length args) ^ " arguments")
    | Isitofp, [|value|] -> typ_str ^ "sitofp " ^ reg_to_string value
    | Isitofp, args -> error ("using sitofp with " ^ string_of_int (Array.length args) ^ " arguments")
    | Igetelementptr, [|addr; offset|] -> typ_str ^ "getelementptr " ^ reg_to_string addr ^ " " ^ reg_to_string offset
    | Igetelementptr, args -> error ("using getelementptr with " ^ string_of_int (Array.length args) ^ " arguments")
    | Icall fn, args ->
        let args = Array.to_list args in
        typ_str ^ "call " ^ reg_to_string fn ^ "(" ^ String.concat " " (List.map reg_to_string args) ^ ")"
    | Iextcall fn, args ->
        let args = Array.to_list args in
        typ_str ^ "c-call " ^ reg_to_string fn ^ "(" ^ String.concat " " (List.map reg_to_string args) ^ ")"
    | Iifthenelse(ifso, ifnot), [|cond|] ->
        "if " ^ reg_to_string cond ^ " then {\n" ^ to_string ifso ^ "} else {\n" ^ to_string ifnot ^ "}"
    | Iifthenelse(_,_), args -> error ("using ifthenelse with " ^ string_of_int (Array.length args) ^ " arguments")
    | Iswitch(indexes, blocks), [|value|] ->
        "switch (" ^ string_of_type typ ^ " " ^ reg_to_string value ^ ") {\n\t" ^
        String.concat "\n\t" (Array.to_list (Array.map to_string blocks)) ^ "}"
    | Iswitch(_,_), args -> error ("using switch with " ^ string_of_int (Array.length args) ^ " arguments")
    | Ireturn, [|value|] ->
        "return " ^ string_of_type typ ^ " " ^ reg_to_string value
    | Ireturn, args -> error ("using return with " ^ string_of_int (Array.length args) ^ " arguments")
    | Iloop instr, [||] -> "loop {\n" ^ to_string instr ^ "}"
    | Iloop _, args -> error ("using loop with " ^ string_of_int (Array.length args) ^ " arguments")
    | Iexit i, [||] -> "goto exit" ^ string_of_int i
    | Iexit _, args -> error ("using exit with " ^ string_of_int (Array.length args) ^ " arguments")
    | Icatch(i, instr1, instr2), [|instr; res|] ->
        "catch " ^ reg_to_string res ^ " = " ^ reg_to_string instr (* TODO figure out what to do here *)
    | Icatch(_,_,_), args -> error ("using catch with " ^ string_of_int (Array.length args) ^ " arguments")
    | Iraise, [|exn|] -> "raise " ^ reg_to_string exn
    | Iraise, args -> error ("using raise with " ^ string_of_int (Array.length args) ^ " arguments")
    | Itrywith(try_instr, with_instr), [||] -> "try {\n" ^ to_string try_instr ^ "} with {\n" ^ to_string with_instr ^ "}" 
    | Itrywith(_,_), args -> error ("using try-with with " ^ string_of_int (Array.length args) ^ " arguments")
    | Ialloc len, [||] -> typ_str ^ "alloc " ^ string_of_int len
    | Ialloc _, args -> error ("using alloc with " ^ string_of_int (Array.length args) ^ " arguments")
    | Iunreachable, [||] -> "unreachable"
    | Iunreachable, args -> error ("using unreachable with " ^ string_of_int (Array.length args) ^ " arguments")
    | Icomment s, [||] -> "comment " ^ s
    | Icomment _, args -> error ("using comment with " ^ string_of_int (Array.length args) ^ " arguments")
    (*| _, args -> error ("unknown instruction with " ^ string_of_int
     * (Array.length args) ^ " arguments")*)
  in
  if foo = "" then foo
  else foo ^ "\n" ^ to_string instr.next
