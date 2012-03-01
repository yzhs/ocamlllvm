open Aux
open Reg
open Mach

let rec instr_to_string instr =
  let typ = instr.typ in
  let res = instr.res in
  let typ_str = string_of_reg res ^ " = " in
  match instr.desc, instr.arg with
    Iend, _ -> ""
  | Ibinop op, [|left; right|] ->
      typ_str ^ string_of_binop op ^ " " ^ reg_name left ^ " " ^ reg_name right
  | Ibinop op, args -> error ("using binop " ^ string_of_binop op ^ " with " ^ string_of_int (Array.length args) ^ " arguments")
  | Icomp op, [|left; right|] ->
      typ_str ^ string_of_comp typ op ^ " " ^ reg_name left ^ " " ^ reg_name right
  | Icomp op, args -> error ("using comp " ^ string_of_comp typ op ^ " with " ^ string_of_int (Array.length args) ^ " arguments")
  | Ialloca, [||] -> string_of_reg res  ^ " = alloca " ^ string_of_type typ
  | Ialloca, args -> error ("using alloca with " ^ string_of_int (Array.length args) ^ " arguments")
  | Iload, [|addr|] -> typ_str ^ "load " ^ string_of_reg addr
  | Iload, args -> error ("using load with " ^ string_of_int (Array.length args) ^ " arguments")
  | Istore, [|value; addr|] ->
       "store " ^ string_of_reg value ^ " " ^ string_of_reg addr
  | Istore, args -> error ("using store with " ^ string_of_int (Array.length args) ^ " arguments")
  | Ifptosi, [|value|] -> typ_str ^ "fptosi " ^ reg_name value
  | Ifptosi, args -> error ("using fptosi with " ^ string_of_int (Array.length args) ^ " arguments")
  | Isitofp, [|value|] -> typ_str ^ "sitofp " ^ reg_name value
  | Isitofp, args -> error ("using sitofp with " ^ string_of_int (Array.length args) ^ " arguments")
  | Igetelementptr, [|addr; offset|] -> typ_str ^ "getelementptr " ^ reg_name addr ^ " " ^ reg_name offset
  | Igetelementptr, args -> error ("using getelementptr with " ^ string_of_int (Array.length args) ^ " arguments")
  | Icall fn, args ->
      let args = Array.to_list args in
      typ_str ^ "call " ^ reg_name fn ^ "(" ^ String.concat " " (List.map string_of_reg args) ^ ")"
  | Iextcall(fn, alloc), args ->
      let args = Array.to_list args in
      typ_str ^ (if not alloc then "noalloc " else "") ^ "c-call " ^ reg_name fn ^ "(" ^ String.concat " " (List.map string_of_reg args) ^ ")"
  | Iifthenelse(ifso, ifnot), [|cond|] ->
      "if returning " ^ string_of_reg res ^ " (" ^ reg_name cond ^ ") then {\n" ^
      instrs_to_string ifso ^ "} else {\n" ^ instrs_to_string ifnot ^ "}"
  | Iifthenelse(_,_), args -> error ("using ifthenelse with " ^ string_of_int (Array.length args) ^ " arguments")
  | Iswitch(indexes, blocks), [|value|] ->
      "switch returning " ^ string_of_reg res ^ " (" ^ string_of_type typ ^ " " ^ reg_name value ^ ") {\ncase:\n" ^
      String.concat "\ncase:\n" (Array.to_list (Array.map instrs_to_string blocks)) ^ "}"
  | Iswitch(_,_), args -> error ("using switch with " ^ string_of_int (Array.length args) ^ " arguments")
  | Ireturn, [|value|] ->
      "return " ^ string_of_type typ ^ " " ^ reg_name value
  | Ireturn, args -> error ("using return with " ^ string_of_int (Array.length args) ^ " arguments")
  | Iloop instr, [||] -> "loop returning " ^ string_of_reg res ^ " {\n" ^ instrs_to_string instr ^ "}"
  | Iloop _, args -> error ("using loop with " ^ string_of_int (Array.length args) ^ " arguments")
  | Iexit i, [||] -> "goto exit" ^ string_of_int i
  | Iexit _, args -> error ("using exit with " ^ string_of_int (Array.length args) ^ " arguments")
  | Icatch(i, instr1, instr2), [||] ->
      "catch returning " ^ string_of_reg res ^ " {\n" ^ instrs_to_string instr1 ^
      "} exit with (" ^ string_of_int i ^ ") {\n" ^ instrs_to_string instr2
  | Icatch(i, instr1, instr2), args -> error ("using catch with " ^ string_of_int (Array.length args) ^ " arguments")
  | Iraise, [|exn|] -> "raise " ^ reg_name exn
  | Iraise, args -> error ("using raise with " ^ string_of_int (Array.length args) ^ " arguments")
  | Itrywith(try_instr, with_instr), [||] ->
      "try returning " ^ string_of_reg res ^ " {\n" ^
      instrs_to_string try_instr ^ "} with {\n" ^ instrs_to_string with_instr ^ "}" 
  | Itrywith(_,_), args -> error ("using try-with with " ^ string_of_int (Array.length args) ^ " arguments")
  | Ialloc len, [||] -> typ_str ^ "alloc " ^ string_of_int len
  | Ialloc _, args -> error ("using alloc with " ^ string_of_int (Array.length args) ^ " arguments")
  | Iunreachable, [||] -> "unreachable"
  | Iunreachable, args -> error ("using unreachable with " ^ string_of_int (Array.length args) ^ " arguments")
  | Icomment s, [||] -> "comment " ^ s
  | Icomment _, args -> error ("using comment with " ^ string_of_int (Array.length args) ^ " arguments")
  (*| _, args -> error ("unknown instruction with " ^ string_of_int
   * (Array.length args) ^ " arguments")*)
and instrs_to_string instr =
  if Iend = instr.desc then ""
  else begin
    instr_to_string instr ^ "\n" ^ instrs_to_string instr.next
  end
