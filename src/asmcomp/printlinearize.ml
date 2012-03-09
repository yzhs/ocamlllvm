open Aux
open Reg
open Mach
open Linearize

let print_array f arr =
  String.concat ", " (Array.to_list (Array.map f arr))

(* Produce a verbose representation of an Instruktion. Used to produce debugging
* output in case something goes wrong when generating LLVM IR. *)
let print_instr instr = begin
  let res = instr.res in
  match instr.desc with
    Lend -> print_string "end"
  | Lop op -> print_string (string_of_reg res ^ " = op " ^ string_of_binop op)
  | Lcomp op -> print_string (string_of_reg res ^ " = comp " ^ string_of_comp (typeof instr.arg.(0)) op)
  | Lcast op -> print_string (string_of_reg res ^ " = cast " ^ string_of_cast op)
  | Lalloca -> print_string (string_of_reg res ^ " = alloca " ^ string_of_type (try deref (typeof res) with Cast_error s -> error ("dereferencing alloca argument " ^ reg_name res ^ " failed")))
  | Lload -> print_string (string_of_reg res ^ " = load")
  | Lstore -> print_string ("store ")
  | Lgetelemptr -> print_string (string_of_reg res ^ " = getelemptr")
  | Lfptosi -> print_string (string_of_reg res ^ " = fptosi")
  | Lsitofp -> print_string (string_of_reg res ^ " = sitofp")
  | Lcall fn -> print_string (string_of_reg res ^ " = call " ^ string_of_reg fn)
  | Lextcall fn -> print_string (string_of_reg res ^ " = extcall " ^ string_of_reg fn)
  | Llabel name -> print_string ("label " ^ name)
  | Lbranch name -> print_string ("branch " ^ name)
  | Lcondbranch(ifso, ifnot) -> print_string ("branch " ^ ifso ^ ", " ^ ifnot)
  | Lswitch(default, lbls) -> print_string ("switch default " ^ default ^ " cases [" ^ print_array (fun x -> x) lbls ^ "]")
  | Lreturn -> print_string ("return")
  | Lunreachable -> print_string ("unreachable")
  | Lcomment _ -> print_string ("comment")
  end;
  print_endline (" (" ^ print_array string_of_reg instr.arg ^ ")")
