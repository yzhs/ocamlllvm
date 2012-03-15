(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Cmm
open Arch
open Emitaux

open Aux
open Reg
open Mach
open Linearize

let error s = error ("Llvmemit: " ^ s)

let emit_nl s = emit_string (s ^ "\n")

let counter = ref 0

let calling_conv = "fastcc"

let counter_inc () = counter := !counter + 1
let c () = counter_inc (); "." ^ string_of_int !counter

let types = Hashtbl.create 10

let emit_label lbl = emit_nl (lbl ^ ":")
let emit_instr instr = emit_nl ("\t" ^ instr)

let emit_op reg op typ args =
  emit_instr (reg_name reg ^ " = " ^ op ^ " " ^ string_of_type typ ^ " " ^
              String.concat ", " (List.map reg_name args))

let arg_list args = String.concat ", " (List.map string_of_reg args)

let emit_cast reg op value typ =
  emit_instr (reg_name reg ^ " = " ^ op ^ " " ^ string_of_reg value ^ " to " ^
              string_of_type (typeof reg))

let rec instr_iter f instr =
  if instr.desc <> Lend then begin
    f instr;
    instr_iter f instr.next
  end

let emit_call res cc fn args =
  let fn = " " ^ reg_name fn ^ "(" ^ Printlinearize.print_array string_of_reg args ^ ") nounwind" in
  emit_instr ((if res <> Nothing then reg_name res ^ " = " else "") ^ "tail call " ^
              cc ^ " " ^ (if res <> Nothing then string_of_type (typeof res) else "void") ^ fn)

let emit_llvm instr =
  let { desc = desc; next = next; arg = arg; res = res; dbg = dbg } = instr in
  begin match desc, arg, res with
    Lend, _, _ -> ()
  | Lop op, [|left; right|], Reg(_, typ) ->
      emit_op res (string_of_binop op) typ [left; right]
  | Lcomp op, [|left; right|], Reg(_, Integer 1) ->
      emit_op res (string_of_comp (typeof left) op) (typeof left) [left; right]
  | Lcast op, [|value|], Reg(_, typ) ->
      emit_cast res (string_of_cast op) value typ
  | Lalloca, [||], Reg(_, typ) ->
      emit_instr (reg_name res ^ " = alloca " ^ string_of_type (deref typ))
  | Lload, [|addr|], Reg(_, _) -> emit_op res "load" (typeof addr) [addr]
  | Lstore, [|value; addr|], Nothing ->
      emit_instr ("store " ^ arg_list [value; addr])
  | Lgetelemptr, [|addr; offset|], Reg(_, _) ->
      emit_instr (reg_name res ^ " = getelementptr " ^ arg_list [addr; offset])
  | Lfptosi, [|value|], Reg(_, typ) -> emit_cast res "fptosi" value typ
  | Lsitofp, [|value|], Reg(_, typ) -> emit_cast res "sitofp" value typ
  | Lcall fn, args, _ -> emit_call res calling_conv fn args
  | Lextcall fn, args, _ -> emit_call res "ccc" fn args
  | Llabel name, [||], Nothing -> emit_label name
  | Lbranch lbl, [||], Nothing -> emit_instr ("br label %" ^ lbl)
  | Lcondbranch(then_label, else_label), [|cond|], Nothing ->
      emit_instr ("br i1 " ^ reg_name cond ^ ", label %" ^ then_label ^ ", label %" ^ else_label)
  | Lswitch(default, lbls), [|value|], Nothing ->
      let typ = string_of_type (typeof value) in
      let fn i lbl = typ ^ " " ^ string_of_int i ^ ", label %" ^ lbl in
      emit_instr ("switch " ^ typ ^ " " ^ reg_name value ^ ", label %" ^
                  default ^ " [\n\t\t" ^
                  String.concat "\n\t\t" (Array.to_list (Array.mapi fn lbls)) ^
                  "\n\t]")
  | Lreturn, [||], Nothing -> emit_instr "ret void"
  | Lreturn, [|value|], Nothing -> emit_instr ("ret " ^ string_of_reg value)
  | Lunreachable, [||], Nothing -> emit_instr "unreachable"
  | Lcomment s, [||], Nothing -> emit_instr ("; " ^ s)

  | Lop op, _, _ -> error ("binop " ^ string_of_binop op ^ " used with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lcomp op, _, _ -> error ("comp " ^ string_of_comp (typeof arg.(0)) op ^ " used with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lcast op, _, _ -> error ("cast " ^ string_of_cast op ^ " used with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lalloca, _, _ -> error ("alloca with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lload, _, _ -> error ("load with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lstore, _, _ -> error ("store with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lgetelemptr, _, _ -> error ("getelemptr with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lfptosi, _, _ -> error ("fptosi with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lsitofp, _, _ -> error ("sitofp with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Llabel name, _, _ -> error ("label with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lbranch lbl, _, _ -> error ("branch with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lcondbranch(then_label, else_label), _, _ -> error ("condbranch with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lswitch(default, lbls), _, _ -> error ("switch with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lreturn, _, _ -> error ("return with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lunreachable, _, _ -> error ("unreachable with " ^ string_of_int (Array.length arg) ^ " arguments")
  | Lcomment s, _, _ -> error ("comment with " ^ string_of_int (Array.length arg) ^ " arguments")
  end

let fundecl = function { fun_name = name; fun_args = args; fun_body = body } ->
  let args = String.concat ", " (List.map string_of_reg args) in
  emit_nl ("define " ^ calling_conv ^ " " ^ string_of_type addr_type ^
           " @" ^ name ^ "(" ^ args ^ ") nounwind noinline gc \"ocaml\" {");
  begin
    try instr_iter emit_llvm body
    with Llvm_error s ->
      print_endline ("emitting code for " ^ name ^ " failed");
      instr_iter Printlinearize.print_instr body;
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
  (*
  (* This is for using the builtin sjlj exception handling *)
  ; "%jump_buf_t = type [5 x " ^ addr_type ^ "]"
  ; "declare i32 @llvm.eh.sjlj.setjmp(i8* ) nounwind"
  ; "declare void @llvm.eh.sjlj.longjmp(i8* ) nounwind"
  *)
  (* This is for the libc sjlj exception handling *)
  ; "%jump_buf_t = type [25 x " ^ addr_type ^ "]"
  ; "declare void @longjmp(i8*, i32) nounwind noreturn"
  ; "declare i32 @setjmp(i8*) nounwind returns_twice"

  ; "declare double @fabs(double) nounwind"
  ; "declare void @llvm.gcroot(i8**, i8*) nounwind"
  ; "declare i8* @llvm.stacksave()"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc1() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc2() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_alloc3() nounwind"
  ; "declare " ^ calling_conv ^ " " ^ addr_type ^ " @caml_allocN(" ^ addr_type ^ ") nounwind"
  ; "declare void @caml_ml_array_bound_error() nounwind"
  ; "declare void @caml_call_gc() nounwind"

  ; "@caml_young_ptr = external global " ^ addr_type
  ; "@caml_young_limit = external global " ^ addr_type
  ; "@caml_bottom_of_stack = external global i8*"
  ; "@caml_last_return_address  = external global i8*" 
  ; "@caml_exn = external global " ^ addr_type
  ; "@caml_jump_buffer = external global %jump_buf_t"
  ]

let constants : string list ref = ref []

let functions : (string * string * string * string list) list ref = ref []

let local_functions = ref []

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
  List.iter (fun name ->
                if not (List.mem name (List.map (fun (_,_,x,_) -> x) !functions)) &&
                   not (List.mem name (List.map fst !local_functions)) then
                  emit_nl ("@" ^ name ^ " = external global " ^ string_of_type int_type))
    !constants

let begin_assembly() = List.iter emit_nl header

let end_assembly() =
  emit_function_declarations ();
  emit_constant_declarations ();
  local_functions := []

(* vim: set foldenable : *)
