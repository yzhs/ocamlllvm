open Cmm
open Emit
open Emitaux
open Llvm_aux
open Llvm_mach
open Llvm_linearize

let error s = error ("Llvmemit: " ^ s)

let emit_nl s = emit_string (s ^ "\n")

let counter = ref 0

let calling_conv = "fastcc"

let counter_inc () = counter := !counter + 1
let c () = counter_inc (); "." ^ string_of_int !counter

let types = Hashtbl.create 10

let translate_symbol s =
  let result = ref "" in 
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      ' ' ->
          result := !result ^ "_"
    | _ -> result := !result ^ Printf.sprintf "%c" c
  done;
  !result

let translate_comp typ =
  match typ with
  | Double -> begin
      function
      | Comp_eq -> "fcmp oeq"
      | Comp_ne -> "fcmp one"
      | Comp_lt -> "fcmp olt"
      | Comp_le -> "fcmp ole"
      | Comp_gt -> "fcmp ogt"
      | Comp_ge -> "fcmp oge"
    end
  | Address _ -> begin
      function
      | Comp_eq -> "icmp  eq"
      | Comp_ne -> "icmp  ne"
      | Comp_lt -> "icmp slt"
      | Comp_le -> "icmp sle"
      | Comp_gt -> "icmp sgt"
      | Comp_ge -> "icmp sge"
    end
  | Integer _ -> begin
      function
      | Comp_eq -> "icmp  eq"
      | Comp_ne -> "icmp  ne"
      | Comp_lt -> "icmp ult"
      | Comp_le -> "icmp ule"
      | Comp_gt -> "icmp ugt"
      | Comp_ge -> "icmp uge"
    end
  | _ -> error "no comparison operations are defined for this type"

let emit_label lbl = emit_nl (lbl ^ ":")
let emit_instr instr = emit_nl ("\t" ^ instr)

let emit_op reg op typ args =
  emit_instr (reg_to_string reg ^ " = " ^ op ^ " " ^ string_of_type typ ^ " " ^
              String.concat ", " (List.map reg_to_string args))

let arg_list args =
  String.concat ", " (List.map (fun x -> string_of_type (typeof x) ^ " " ^ reg_to_string x) args)

let emit_cast reg op value typ =
  emit_instr (reg_to_string reg ^ " = " ^ op ^ " " ^ string_of_type (typeof value) ^
              " " ^ reg_to_string value ^ " to " ^ string_of_type (typeof reg))

let rec instr_iter f instr =
  match instr.desc with
    Lend -> ()
  | _ -> f instr; instr_iter f instr.next

let print_reg reg = string_of_type (typeof reg) ^ " " ^ reg_to_string reg

let print_array f arr =
  String.concat ", " (Array.to_list (Array.map f arr))

let to_string instr = begin
  let res = instr.res in
  match instr.desc with
    Lend -> print_string "end"
  | Lop op -> print_string (print_reg res ^ " = op " ^ string_of_binop op)
  | Lcomp op -> print_string (print_reg res ^ " = comp " ^ string_of_comp (typeof instr.arg.(0)) op)
  | Lcast op -> print_string (print_reg res ^ " = cast " ^ string_of_cast op)
  | Lalloca -> print_string (print_reg res ^ " = alloca " ^ string_of_type (try deref (typeof res) with Cast_error s -> error ("dereferencing alloca argument " ^ reg_to_string res ^ " failed")))
  | Lload -> print_string (print_reg res ^ " = load")
  | Lstore -> print_string ("store ")
  | Lgetelemptr -> print_string (print_reg res ^ " = getelemptr")
  | Lfptosi -> print_string (print_reg res ^ " = fptosi")
  | Lsitofp -> print_string (print_reg res ^ " = sitofp")
  | Lcall fn -> print_string (print_reg res ^ " = call " ^ print_reg fn)
  | Lextcall fn -> print_string (print_reg res ^ " = extcall " ^ print_reg fn)
  | Llabel name -> print_string ("label " ^ name)
  | Lbranch name -> print_string ("branch " ^ name)
  | Lcondbranch(ifso, ifnot) -> print_string ("branch " ^ ifso ^ ", " ^ ifnot)
  | Lswitch(default, lbls) -> print_string ("switch default " ^ default ^ " cases [" ^ print_array (fun x -> x) lbls ^ "]")
  | Lreturn -> print_string ("return")
  | Lunreachable -> print_string ("unreachable")
  | Lcomment _ -> print_string ("comment")
  end;
  print_endline (" (" ^ print_array print_reg instr.arg ^ ")")

let emit_call res cc fn args =
  let reg_type reg = string_of_type (typeof reg) in
  let fn = " " ^ reg_to_string fn ^ " (" ^ print_array (fun x -> reg_type x ^ " " ^ reg_to_string x) args ^ ") nounwind" in
  emit_instr ((if res <> Nothing then reg_to_string res ^ " = " else "") ^ "call " ^
              cc ^ " " ^ (if res <> Nothing then reg_type res else "void") ^ fn)

let emit_llvm instr =
  let { desc = desc; next = next; arg = arg; res = res; dbg = dbg } = instr in begin
  match desc, arg, res with
    Lend, _, _ -> ()
  | Lop op, [|left; right|], Reg(_, typ) ->
      emit_op res (string_of_binop op) typ [left; right]
  | Lcomp op, [|left; right|], Reg(_, Integer 1) ->
      emit_op res (string_of_comp (typeof left) op) (typeof left) [left; right]
  | Lcast op, [|value|], Reg(_, typ) ->
      emit_cast res (string_of_cast op) value typ
  | Lalloca, [||], Reg(_, typ) ->
      emit_instr (reg_to_string res ^ " = alloca " ^ string_of_type (try deref typ with Cast_error s -> error "dereferencing result type of Lalloca failed"))
  | Lload, [|addr|], Reg(_, typ) ->
      emit_op res "load" (typeof addr) [addr]
  | Lstore, [|value; addr|], Nothing ->
      emit_instr ("store " ^ arg_list [value; addr])
  | Lgetelemptr, [|addr; offset|], Reg(_, typ) ->
      emit_instr (reg_to_string res ^ " = getelementptr " ^ arg_list [addr; offset])
  | Lfptosi, [|value|], Reg(name, typ) -> emit_cast res "fptosi" value typ
  | Lsitofp, [|value|], Reg(name, typ) -> emit_cast res "sitofp" value typ
  | Lcall fn, args, _ -> emit_call res calling_conv fn args
  | Lextcall fn, args, _ -> emit_call res "ccc" fn args
  | Llabel name, [||], Nothing -> emit_label name
  | Lbranch lbl, [||], Nothing -> emit_instr ("br label %" ^ lbl)
  | Lcondbranch(then_label, else_label), [|cond|], Nothing ->
      emit_instr ("br i1 " ^ reg_to_string cond ^ ", label %" ^ then_label ^ ", label %" ^ else_label)
  | Lswitch(default, lbls), [|value|], Nothing ->
      let typ = string_of_type (typeof value) in
      let fn i lbl = typ ^ " " ^ string_of_int i ^ ", label %" ^ lbl in
      emit_instr ("switch " ^ typ ^ " " ^ reg_to_string value ^ ", label %" ^
                  default ^ " [\n" ^
                  String.concat "\n\t\t" (Array.to_list (Array.mapi fn lbls)) ^
                  "\t]")
  | Lreturn, [||], Nothing -> emit_instr "ret void"
  | Lreturn, [|value|], Nothing ->
      emit_instr ("ret " ^ string_of_type (typeof value) ^ " " ^ reg_to_string value)
  | Lunreachable, [||], Nothing -> emit_instr "unreachable"
  | Lcomment s, [||], Nothing -> emit_instr ("; " ^ s)
  | _, _, _ -> error "unknown instruction"
  end

(*
and call cc ret fn args =
  let args =
    let fn x =
      match emit_llvm x with
      | Just s -> string_of_type (typeof x) ^ " " ^ s
      | Error s -> error ("failed to emit code for arguments of call:\n" ^ s)
    in
    String.concat ", " (List.map fn args)
  in
  let f fn =
    if ret == Void then begin
      emit_instr ("call " ^ cc ^ " " ^ string_of_type ret ^ " " ^ fn ^ "(" ^ args ^ ") nounwind");
      fail "void function does not return anything";
    end else begin
      let result = "%result" ^ c() in
      emit_instr (result ^ " = call " ^ cc ^ " " ^ string_of_type ret ^ " " ^ fn ^ "(" ^ args ^ ") nounwind");
      return result
    end
  in
  emit_llvm fn >>= f
 *)

let fundecl = function { fun_name = name; fun_args = args; fun_body = body } ->
  let fn reg = string_of_type (typeof reg) ^ " " ^ reg_to_string reg in
  let args = String.concat ", " (List.map fn args) in
  emit_nl ("define " ^ calling_conv ^ " " ^ string_of_type addr_type ^
           " @" ^ name ^ "(" ^ args ^ ") nounwind gc \"ocaml\" {");
  begin
    try instr_iter emit_llvm body
    with Llvm_error s ->
      print_endline ("emitting code for " ^ name ^ " failed");
      instr_iter to_string body;
      error s
  end;
  emit_nl "}\n"


(*
 * Header with declarations of some functions and constants needed in every
 * module.
 *)
let header =
  let addr_type = string_of_type addr_type in
  [ "; vim: set ft=llvm:"
  ; "%jump_buf_t = type [5 x " ^ addr_type ^ "]"
  ; "declare double @fabs(double) nounwind"
  ; "declare void @llvm.gcroot(i8**, i8*) nounwind"
  ; "declare i32 @llvm.eh.sjlj.setjmp(i8*) nounwind"
  ; "declare void @llvm.eh.sjlj.longjmp(i8*) nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc1() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc2() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc3() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_allocN(" ^ addr_type ^ ") nounwind"
  ; "declare void @caml_ml_array_bound_error() nounwind"
  ; "declare void @caml_call_gc() nounwind"
  ; "@caml_exception_pointer = external global " ^ addr_type
  ; "@caml_young_ptr = external global " ^ addr_type
  ; "@caml_young_limit = external global " ^ addr_type
  ; "@caml_bottom_of_stack = external global " ^ addr_type
  ; "@caml_last_return_address  = external global " ^ addr_type
  ; "@exn = external global " ^ addr_type 
  ; "@jmp_buf = external global %jump_buf_t"
  ]

let constants : string list ref = ref []

let functions : (string * string * string * string list) list ref = ref []

let local_functions = ref []

let module_asm () = emit_string "module asm \""

let add_const str =
  if List.exists (fun x -> String.compare x str == 0) !constants
  then ()
  else constants := str :: !constants

let add_function (ret, cconv, str, args) =
  if List.exists (fun (_, _, x, _) -> String.compare x str == 0) !functions
  then ()
  else functions := (string_of_type ret, cconv, str, List.map (fun _ -> string_of_type addr_type) args) :: !functions

let emit_function_declarations () =
  let fn (ret_type, cconv, name, args) =
    emit_nl ("declare " ^ cconv ^ " " ^ ret_type ^ " @" ^ name ^
             "(" ^ String.concat "," args ^ ") nounwind")
  in
  List.iter fn (List.filter (fun (_, _, name, _) -> not (List.mem name (List.map fst !local_functions))) !functions)

let emit_constant_declarations () =
  List.iter (fun name -> if List.mem name (List.map (fun (_,_,x,_) -> x) !functions) then () else emit_nl ("@" ^ name ^ " = external global " ^ string_of_type int_type)) !constants


(* Emission of data *)

let macosx =
  match Config.system with
  | "macosx" -> true
  | _ -> false

let emit_symbol s =
  if macosx then emit_string "_";
  Emitaux.emit_symbol '$' s

let emit_align n =
    let n = if macosx then Misc.log2 n else n in
  emit_string "module asm \"        .align  "; emit_int n; emit_string "\"\n"

let emit_string_literal s =
  let last_was_escape = ref false in
  emit_string "\\22";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.fprintf !output_channel "\\x%x" (Char.code c)
      else output_char !output_channel c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      output_char !output_channel c;
      last_was_escape := false
    end else begin
      Printf.fprintf !output_channel "\\x%x" (Char.code c);
      last_was_escape := true
    end
  done;
  emit_string "\\22"

let emit_string_directive directive s =
  let l = String.length s in
  if l = 0 then ()
  else if l < 80 then begin
    module_asm();
    emit_string directive;
    emit_string_literal s;
    emit_string "\"\n"
  end else begin
    let i = ref 0 in
    while !i < l do
      module_asm();
      let n = min (l - !i) 80 in
      emit_string directive;
      emit_string_literal (String.sub s !i n);
      emit_string "\"\n";
      i := !i + n
    done
  end


let emit_item =
  let emit_label l = emit_string (".L" ^ string_of_int l) in
  function
    Cglobal_symbol s ->
      (emit_string "module asm \"\t.globl  "; emit_symbol s; emit_string "\"\n");
  | Cdefine_symbol s ->
      (module_asm(); emit_symbol s; emit_string ":\"\n")
  | Cdefine_label lbl ->
      (module_asm(); emit_label (100000 + lbl); emit_string ":\"\n")
  | Cint8 n ->
      (emit_string "module asm \"\t.byte   "; emit_int n; emit_string "\"\n")
  | Cint16 n ->
      (emit_string "module asm \"\t.word   "; emit_int n; emit_string "\"\n")
  | Cint32 n ->
      (emit_string "module asm \"\t.long   "; emit_nativeint n; emit_string "\"\n")
  | Cint n ->
      (emit_string "module asm \"\t.quad   "; emit_nativeint n; emit_string "\"\n")
  | Csingle f ->
      module_asm(); emit_float32_directive ".long" f; emit_string "\""
  | Cdouble f ->
      module_asm(); emit_float64_directive ".quad" f; emit_string "\""
  | Csymbol_address s ->
      (emit_string "module asm \"\t.quad   "; emit_symbol s; emit_string "\"\n")
  | Clabel_address lbl ->
      (emit_string "module asm \"\t.quad   "; emit_label (100000 + lbl); emit_string "\"\n")
  | Cstring s ->
      emit_string_directive "\t.ascii  " s
  | Cskip n ->
      if n > 0 then (emit_string "module asm \"\t.space  "; emit_int n; emit_string "\"\n")
  | Calign n -> emit_align n

let data l =
  emit_nl "module asm \"\t.data\"";
  List.iter emit_item l

let begin_assembly() = List.iter emit_nl header

let end_assembly() =
  emit_function_declarations ();
  emit_constant_declarations ();
  local_functions := []

(* vim: set foldenable : *)
