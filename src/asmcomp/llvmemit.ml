open Cmm
open Emit
open Emitaux
open Llvm_types

let emit_nl s = emit_string (s ^ "\n")

let counter = ref 0

let calling_conv = "fastcc"

let counter_inc () = counter := !counter + 1
let c () = counter_inc (); "." ^ string_of_int !counter

(* {{{ *)
(* Print an expression in the intermediate format using a syntax inspired by
 * S-expressions *)
let rec to_string = function
  | Lvar(name, typ) -> "(var " ^ string_of_type typ ^ " " ^ name ^ ")"
  | Lbinop(op, typ, left, right) ->
      "(" ^ string_of_binop op ^ " " ^ string_of_type typ ^ " " ^ to_string left ^ " " ^ to_string right ^ ")"
  | Lcomp(op, typ, left, right) ->
      "(" ^ op ^ " " ^ string_of_type typ ^ " " ^ to_string left ^ " " ^ to_string right ^ ")"
  | Lalloca(name, typ) -> "(alloca " ^ name ^ " " ^ string_of_type typ ^ ")"
  | Lload addr -> "(load " ^ to_string addr ^ ")"
  | Lstore(value, addr) ->
      "(store " ^ to_string value ^ " " ^ to_string addr ^ ")"
  | Lcast(cast, value, from_type, to_type) ->
      "(" ^ string_of_cast cast ^ " " ^ to_string value ^ " " ^ string_of_type from_type ^ " " ^ string_of_type to_type ^ ")"
  | Lgetelementptr(addr, offset) -> "(elemptr " ^ to_string addr ^ " " ^ to_string offset ^ ")"
  | Lcall(ret, fn, args) ->
      "(call " ^ string_of_type addr_type ^ " " ^ to_string fn ^ "(" ^ String.concat " " (List.map to_string args) ^ "))"
  | Lccall(ret, fn, args) ->
      "(call ccc " ^ string_of_type addr_type ^ " " ^ to_string fn ^ "(" ^ String.concat " " (List.map to_string args) ^ "))"
  | Lconst(const, typ) -> "(const " ^ const ^ " " ^ string_of_type typ ^ ")"
  | Llabel name -> "(label " ^ name ^ ")"
  | Lbr label_name -> "(br " ^ label_name ^ ")"
  | Lbr_cond(cond, then_label, else_label) ->
      "(if " ^ to_string cond ^ " (br " ^ then_label ^ ") (br " ^ else_label ^ "))"
  | Lswitch(c, value, indexes, _, blocks, typ) ->
      "(switch " ^ string_of_type typ ^ " " ^ to_string value ^ " of " ^
      String.concat ", " (Array.to_list (Array.map to_string blocks)) ^ ")"
  | Lreturn(value, typ) ->
      "(return " ^ string_of_type typ ^ " " ^ to_string value ^ ")"
  | Lseq(instr1,instr2) -> to_string instr1 ^ ";\n\t" ^ to_string instr2
  | Lcaml_raise_exn exn -> "(raise " ^ to_string exn ^ ")"
  | Lcaml_catch_exn(id, instr, res) ->
      "(catch " ^ id ^ " storing " ^ to_string instr ^ " in " ^ to_string res ^ ")"
  | Lcaml_alloc len -> "(ALLOC " ^ string_of_int len ^ ")"
  | Ldefine(name, args, body) ->
      "(define " ^ name ^ " " ^ String.concat " " (List.map (fun (x,_) -> x) args) ^ " " ^ to_string body ^ ")"
  | Lnothing -> "nothing"
  | Lunreachable -> "unreachable"
  | Lcomment s -> "(comment " ^ s ^ ")"
(* }}} *)

let debug = ref true

let print_debug str = if !debug then print_endline str

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

let emit_label lbl = emit_nl (lbl ^ ":")
let emit_instr instr = emit_nl ("\t" ^ instr)

let rec lower instr =
  match instr with
  | Lcaml_raise_exn exn ->
      Lcall(Void, Lvar("@caml_raise_exn", Function(Void, [addr_type])), [exn]) @@ Lunreachable
  | Lcaml_catch_exn(e, instr, res) -> Lnothing (* FIXME not implemented *)
  | Lcaml_alloc len ->
      (*
      if len = 2 then Lccall(addr_type, Lvar("@caml_alloc1", Any), [])
      else if len = 3 then Lccall(addr_type, Lvar("@caml_alloc2", Any), [])
      else if len = 4 then Lccall(addr_type, Lvar("@caml_alloc3", Any), [])
      else Lccall(addr_type, Lvar("@caml_allocN", Any), [Lcast(Inttoptr, Lconst(string_of_int (len-1), int_type), int_type, addr_type)])
       *)
      (* TODO rewrite the code so it does not create a loop *)
      (* TODO tell LLVM that the garbage collection is unlikely *)
      let young_ptr = Lload(Lvar("@caml_young_ptr", Address addr_type)) in
      let new_young = "nyp" ^ c() in
      let begin_lbl = "begin_alloc" ^ c() in
      let run_gc_lbl = "run_gc" ^ c() in
      let continue_lbl = "continue" ^ c() in
      let offset = string_of_int (-len) in
      let new_young_ptr = Lload(Lvar("%" ^ new_young, Address addr_type)) in
      let limit = Lload(Lvar("@caml_young_limit", Address addr_type)) in
      let cmp_res = Lcomp("icmp ult", addr_type, new_young_ptr, limit) in
      Lcomment ("allocating " ^ string_of_int len ^ " bytes")
      @@ Lbr begin_lbl
      @@ Llabel begin_lbl
      @@ Lstore(Lgetelementptr(young_ptr, Lconst(offset, int_type)), Lalloca(new_young, addr_type))
      @@ Lbr_cond(cmp_res, run_gc_lbl, continue_lbl)
      @@ Llabel run_gc_lbl
      @@ Lcall(Void, Lvar("@caml_call_gc", Any), [])
      @@ Lbr (*continue_lbl*) begin_lbl
      @@ Llabel continue_lbl
      @@ Lstore (new_young_ptr, Lvar("@caml_young_ptr", Address addr_type))
      @@ new_young_ptr
  | Lbinop(op, typ, left, right) -> Lbinop(op, typ, lower left, lower right)
  | Lcomp(op, typ, left, right) -> Lcomp(op, typ, lower left, lower right)
  | Lload instr -> Lload (lower instr)
  | Lstore(src, dest) -> Lstore(lower src, lower dest)
  | Lcast(cast, value, src_type, dest_type) -> Lcast(cast, lower value, src_type, dest_type)
  | Lgetelementptr(address, offset) -> Lgetelementptr(lower address, lower offset) 
  | Lcall(ret_type, fn, args) -> Lcall(ret_type, lower fn, List.map lower args)
  | Lccall(ret_type, fn, args) -> Lccall(ret_type, lower fn, List.map lower args)
  | Lbr_cond(cond, lbl_true, lbl_false) -> Lbr_cond(lower cond, lbl_true, lbl_false)
  | Lswitch(counter, value, indexes, default, blocks, typ) ->
      Lswitch(counter, lower value, indexes, default, Array.map lower blocks, typ)
  | Lreturn(value, typ) -> Lreturn(lower value, typ)
  | Lseq(instr1, instr2) -> Lseq(lower instr1, lower instr2)
  | Ldefine(name, args, body) -> Ldefine(name, args, lower body)
  | Lvar(_,_) | Lalloca(_,_) | Lconst(_,_) | Lbr _ | Lnothing | Lunreachable | Lcomment _ | Llabel _ -> instr


let rec emit_llvm instr =
  match lower instr with
  | Lvar(name, typ) -> return name
  | Lbinop(op, typ, left, right) ->
      emit_op (string_of_binop op) (string_of_type typ) left right
  | Lcomp(op, typ, left, right) ->
      emit_op op (string_of_type typ) left right
  | Lalloca(name, typ) ->
      emit_instr ("%" ^ name ^ " = alloca " ^ string_of_type typ);
      return ("%" ^ name)
  | Lload addr -> emit_unop "load" (string_of_type (typeof addr)) addr
  | Lstore(value, addr) ->
      let fn (v,a) =
        emit_instr ("store " ^ string_of_type (typeof value) ^ " " ^ v ^ ", " ^
                    string_of_type (typeof addr) ^ " " ^ a);
        fail "store does not return anything"
      in
      emit_llvm value ++ emit_llvm addr >>= fn
  | Lcast(op, value, from_type, to_type) ->
      cast value (string_of_cast op) from_type to_type
  | Lgetelementptr(addr, offset) ->
      let name = "%elemptr" ^ c() in
      let offset_type = string_of_type (typeof offset) in
      let addr_type = string_of_type (typeof addr) in
      let fn (addr,offset) =
        emit_instr (name ^ " = getelementptr " ^ addr_type ^ " " ^ addr ^ ", " ^
                    offset_type ^ " " ^ offset);
        return name
      in
      emit_llvm addr ++ emit_llvm offset >>= fn
  | Lcall(ret, fn, args) -> call calling_conv ret fn args
  | Lccall(ret, fn, args) -> call "ccc" ret fn args
  | Lconst(const, _) -> return const
  | Llabel name ->
      emit_label name;
      fail "label does not return anything"
  | Lbr label_name ->
      emit_instr ("br label %" ^ label_name);
      fail "br does not return anything"
  | Lbr_cond(cond, then_label, else_label) ->
      let typ = string_of_type (typeof cond) in
      let fn cond =
        emit_instr ("br " ^ typ ^ " " ^ cond ^ ", label %" ^ then_label ^
                    ", label %" ^ else_label);
        fail "br_cond deos not return anything"
      in
      emit_llvm cond >>= fn
  | Lswitch(c, value, indexes, default, blocks, typ) ->
      let fn value =
        let f i index =
          string_of_type int_type ^ " " ^ string_of_int i ^ ", label %case" ^
          string_of_int index ^ "." ^ c
        in
        let labels = String.concat "\n\t\t" (Array.to_list (Array.mapi f indexes)) in
        emit_instr ("switch " ^ string_of_type int_type ^ " " ^ value ^
                    ", label %default." ^ c ^ " [" ^ labels ^ "]");
        fail "switch does not return anything"
      in
      ignore (emit_llvm value >>= fn);
      ignore (emit_llvm default);
      Array.iter (fun block -> ignore (emit_llvm block)) blocks;
      fail "switch does not return anyting"
  | Lreturn(value, Void) ->
      emit_instr "ret void";
      fail "return statement does not write into any SSA registers"
  | Lreturn(value, _) -> emit_return value
  | Lseq(instr1,Lnothing) -> emit_llvm instr1
  | Lseq(instr1,Lcomment s) -> let res = emit_llvm instr1 in ignore (emit_llvm (Lcomment s)); res
  | Lseq(instr1,instr2) -> ignore (emit_llvm instr1); emit_llvm instr2
  | Ldefine(name, args, body) ->
      let fn (name, typ) = string_of_type typ ^ " " ^ name in
      let args = String.concat ", " (List.map fn args) in
      emit_nl ("define " ^ calling_conv ^ " " ^ string_of_type addr_type ^
               " @" ^ name ^ "(" ^ args ^ ") nounwind gc \"ocaml\" {");
      ignore (emit_llvm body);
      emit_nl "}\n";
      fail "define does not return anything"
  | Lnothing -> fail "nothing"
  | Lunreachable ->
      emit_instr "unreachable";
      fail "unreachable does not return anything"
  | Lcomment s ->
      emit_instr ("; " ^ s);
      fail "comment does not return anything"
  | Lcaml_raise_exn _ | Lcaml_catch_exn _ | Lcaml_alloc _ ->
      error ("lowering instruction failed: " ^ to_string (lower instr))

and emit_op op typ left right =
  let fn (left,right) =
    let res = "%" ^ translate_symbol op ^ "_res" ^ c () in
    emit_nl ("\t" ^ res ^ " = " ^ op ^ " " ^ typ ^ " " ^ left ^ ", " ^ right);
    return res
  in
  emit_llvm left ++ emit_llvm right >>= fn

and emit_unop op typ arg =
  let fn arg =
    let res = "%" ^ translate_symbol op ^ "_res" ^ c () in
    emit_instr (res ^ " = " ^ op ^ " " ^ typ ^ " " ^ arg);
    return res
  in
  emit_llvm arg >>= fn

and cast value op from_type to_type =
  let fn value =
    let res = "%" ^ translate_symbol op ^ "_res" ^ c () in
    emit_instr (res ^ " = " ^ op ^ " " ^ (string_of_type from_type) ^ " " ^ value ^ " to " ^ string_of_type to_type);
    return res
  in
  emit_llvm value >>= fn

and emit_return value =
  let typ = string_of_type (typeof value) in
  let fn value =
    emit_instr ("ret " ^ typ ^ " " ^ value);
    fail "return statement does not write into any SSA register"
  in
  emit_llvm value >>= fn

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


(*
 * Header with declarations of some functions and constants needed in every
 * module.
 *)
let header =
  let addr_type = string_of_type addr_type in
  [ "; vim: set ft=llvm:"
  ; "declare double @fabs(double) nounwind"
  ; "declare void @caml_raise_exn(" ^ addr_type ^ ") noreturn nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc1() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc2() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc3() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_allocN(" ^ addr_type ^ ") nounwind"
  ; "declare ccc void @caml_call_gc() nounwind"
  ; "@caml_exception_pointer = external global " ^ addr_type ^ ""
  ; "@caml_young_ptr = external global " ^ addr_type ^ ""
  ; "@caml_young_limit = external global " ^ addr_type ^ ""
  ; "@caml_bottom_of_stack = external global " ^ addr_type ^ ""
  ; "@caml_last_return_address  = external global " ^ addr_type ^ ""
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
  List.iter (fun name -> emit_nl ("@" ^ name ^ " = external global " ^ string_of_type int_type)) !constants


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


let run_assembler asm infile outfile =
  Ccomp.command (asm ^ " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let assemble_file temp1 temp2 infile outfile =
  let res = run_assembler Config.opt infile temp1 in 
  if res <> 0 then res else
  let res = run_assembler Config.llc temp1 temp2 in 
  if res <> 0 then res else
    run_assembler Config.asm temp2 outfile

(* vim: set foldenable : *)
