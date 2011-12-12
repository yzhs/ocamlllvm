open Cmm
open Emit
open Emitaux

exception Llvm_error of string

type 'a error = Just of 'a | Error of string

let error str = raise (Llvm_error str)

let emit_nl s = emit_string (s ^ "\n")

let counter = ref 0

let size_int = 8 * Arch.size_int
let size_float = 8 * Arch.size_float

let calling_conv = "cc 11"

type llvm_type =
  | Integer of int (* bitwidth *)
  | Double
  | Address of llvm_type
  | Return3
  | Return2
  | Void
  | Any
  | Function of llvm_type * llvm_type list (* return type, argument types *)

let int_type = Integer size_int
let addr_type = Address int_type
let float_sized_int = Integer size_float

let counter_inc () = counter := !counter + 1
let c () = counter_inc (); "." ^ string_of_int !counter

let rec typename = function
  | Integer i -> "i" ^ string_of_int i
  | Double -> "double"
  | Address typ -> typename typ ^ "*"
  | Return3 -> "%res"
  | Return2 -> "%res2"
  | Void -> "void"
  | Any -> (*error "unable to infer type"*) "any"
  | Function(ret, args) -> typename ret ^ " (" ^ String.concat ", " (List.map typename args) ^ ")"

let ret_type = let typ = typename addr_type in "{" ^ typ ^ ", " ^ typ ^ ", " ^ typ ^ "}"

let deref typ = match typ with
  | Address typ -> typ
  | _ -> error ("trying to dereference non-pointer type " ^ typename typ)

type llvm_instr =
  | Lvar of string * llvm_type (* name, type *)
  | Lbinop of string * llvm_type * llvm_instr * llvm_instr (* op, typ, left, right *)
  | Lcomp of string * llvm_type * llvm_instr * llvm_instr (* op, typ, left, right *)
  | Lunop of string * llvm_type * llvm_instr (* op, typ, arg *)
  | Lalloca of string * llvm_type (*name, typ*)
  | Lload of llvm_instr (* address *)
  | Lstore of llvm_instr * llvm_instr (* value, address *)
  | Lzext of llvm_instr * llvm_type * llvm_type (* value, source type, target type *)
  | Ltrunc of llvm_instr * llvm_type * llvm_type (* value, source type, target type *)
  | Lbitcast of llvm_instr * llvm_type * llvm_type (* value, source type, target type *)
  | Linttoptr of llvm_instr * llvm_type * llvm_type (* value, source type, target type *)
  | Lptrtoint of llvm_instr * llvm_type * llvm_type (* value, source type, target type *)
  | Lsitofp of llvm_instr * llvm_type * llvm_type (* value, source type, target type *)
  | Lfptosi of llvm_instr * llvm_type * llvm_type (* value, source type, target type *)
  | Lgetelementptr of llvm_instr * llvm_instr (* address, offset *)
  | Lcall of llvm_type * llvm_instr * llvm_instr list (* return type, name, arguments *)
  | Ltailcall of llvm_type * llvm_instr * llvm_instr list (* return type, name, arguments *)
  | Lccall of llvm_type * llvm_instr * llvm_instr list (* return type, name, arguments; using c calling convention *)
  | Lconst of string * llvm_type (* literal, type *)
  | Llabel of string (* name *)
  | Lbr of string (* label *)
  | Lbr_cond of llvm_instr * string * string (* condition, then label, else label *)
  | Lswitch of string * llvm_instr * int list * llvm_instr * llvm_type (* indexes, blocks *)
  | Lreturn of llvm_instr * llvm_type (* value, type *)
  | Lseq of llvm_instr * llvm_instr (* value, type *)
  | Lcaml_raise_exn of llvm_instr (* argument *)
  | Lcaml_catch_exn of string (* TODO figure out what information is needed *)
  | Lcaml_alloc of int (* length *)
  | Ldefine of string * (string * llvm_type) list * llvm_instr (* name, arguments, body *)
  | Lnothing
  | Lunreachable
  | Lcomment of string

let rec typeof = function
  | Lvar(_, typ) -> typ
  | Lbinop(_,typ,_,_) -> typ
  | Lcomp(_,_,_,_) -> Integer 1
  | Lunop(_,typ,_) -> typ
  | Lalloca(_, typ) -> Address typ
  | Lload addr -> deref (typeof addr)
  | Lstore(_,_) -> Void
  | Lzext(_,_,typ) -> typ
  | Ltrunc(_,_,typ) -> typ
  | Lbitcast(_,_,typ) -> typ
  | Linttoptr(_,_,typ) -> typ
  | Lptrtoint(_,_,typ) -> typ
  | Lsitofp(_,_,typ) -> typ
  | Lfptosi(_,_,typ) -> typ
  | Lgetelementptr(ptr,_) -> typeof ptr
  | Lcall(typ,_,_) -> if typ == Return3 then addr_type else typ
  | Ltailcall(typ,_,_) -> if typ == Return3 then addr_type else typ
  | Lccall(typ,_,_) -> typ
  | Lconst(_,typ) -> typ
  | Llabel _ -> error "Label does not have a type"
  | Lbr _ -> Void
  | Lbr_cond(_,_,_) -> Void
  | Lswitch(_,_,_,_,typ) -> typ
  | Lreturn(_, typ) -> typ
  | Lseq(instr,Lcomment _) -> typeof instr
  | Lseq(_,instr) -> typeof instr
  | Lcaml_raise_exn _ -> Void
  | Lcaml_catch_exn _ -> Void
  | Lcaml_alloc _ -> addr_type
  | Ldefine(_,_,_) -> Return3
  | Lnothing -> error "Lnothing does not have a type"
  | Lunreachable -> error "Lunreachable does not have a type"
  | Lcomment _ -> error "Lcomment does not have a type"

let (@@) a b = Lseq(a, b)

let types = Hashtbl.create 10

let llvm_instrs : llvm_instr list ref = ref []

let (>>=) value fn = match value with
  | Just value -> fn value
  | Error s -> Error s

let just fn x = Just (fn x)

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

let (++) a b = match a, b with
  | Just a, Just b -> Just (a,b)
  | Error e, _ -> Error e
  | Just _, Error e -> Error e

let exn_ptr = Lload(Lvar("%exn_ptr", Address addr_type))
let young_ptr = Lload(Lvar("%young_ptr", Address addr_type))

let emit_label lbl = emit_nl (lbl ^ ":")
let emit_instr instr = emit_nl ("\t" ^ instr)

let rec emit_llvm instr =
  match instr with
  | Lvar(name, typ) -> Just name
  | Lbinop(op, typ, left, right) -> emit_binop op (typename typ) left right
  | Lcomp(op, typ, left, right) -> emit_binop op (typename typ) left right
  | Lunop(op, typ, arg) -> emit_unop op (typename typ) arg
  | Lalloca(name, typ) -> emit_instr ("%" ^ name ^ " = alloca " ^ typename typ); Just ("%" ^ name)
  | Lload addr -> emit_unop "load" (typename (typeof addr)) addr
  | Lstore(value, addr) ->
      let fn (v,a) =
        emit_instr ("store " ^ typename (typeof value) ^ " " ^ v ^ ", " ^ typename (typeof addr) ^ " " ^ a);
        Error "store does not return anything"
      in
      emit_llvm value ++ emit_llvm addr >>= fn
  | Lzext(value, from_type, to_type) -> cast value "zext" from_type to_type
  | Ltrunc(value, from_type, to_type) -> cast value "trunc" from_type to_type
  | Lbitcast(value, from_type, to_type) -> cast value "bitcast" from_type to_type
  | Linttoptr(value, from_type, to_type) -> cast value "inttoptr" from_type to_type
  | Lptrtoint(value, from_type, to_type) -> cast value "ptrtoint" from_type to_type
  | Lsitofp(value, from_type, to_type) -> cast value "sitofp" from_type to_type
  | Lfptosi(value, from_type, to_type) -> cast value "fptosi" from_type to_type
  | Lgetelementptr(addr, offset) ->
      let name = "%elemptr" ^ c() in
      let offset_type = typename (typeof offset) in
      let addr_type = typename (typeof addr) in
      let fn (addr,offset) =
        emit_instr (name ^ " = getelementptr " ^ addr_type ^ " " ^ addr ^ ", " ^ offset_type ^ " " ^ offset);
        name
      in
      emit_llvm addr ++ emit_llvm offset >>= just fn
  | Lcall(ret, fn, args) ->
      let args =
        let fn x =
          match emit_llvm x with
          | Just s -> typename addr_type ^ " " ^ s
          | Error s -> error ("failed to emit code for arguments of caml call:\n" ^ s)
        in
        List.map fn args
      in
      let exn_ptr = emit_llvm (Lload(Lvar("%exn_ptr", Address addr_type))) in
      let young_ptr = emit_llvm (Lload(Lvar("%young_ptr", Address addr_type))) in
      let args = String.concat ", "
        begin
          match exn_ptr, young_ptr with
          | Just e, Just y -> (typename addr_type ^ " " ^ e) :: (typename addr_type ^ " " ^ y) :: args
          | _, _ -> error ("failed to emit code for exn or young pointer")
        end
      in
      let f fn =
        if ret == Void then begin (* TODO even void functions should return caml_young_ptr and caml_exn_ptr *)
          let result = "%result" ^ c() in
          emit_instr (result ^ " = call " ^ calling_conv ^ " %res2 " ^ fn ^ "(" ^ args ^ ")");
          let exn_ptr = extractvalue ("%new_exn_ptr" ^ c()) Return2 result (Lconst("0", int_type)) in
          let young_ptr = extractvalue ("%new_young_ptr" ^ c()) Return2 result (Lconst("1", int_type)) in
          let fn (exn_ptr, young_ptr) =
            emit_instr ("store " ^ typename addr_type ^ " " ^ exn_ptr ^ ", " ^ typename (Address addr_type) ^ "%exn_ptr");
            emit_instr ("store " ^ typename addr_type ^ " " ^ young_ptr ^ ", " ^ typename (Address addr_type) ^ "%young_ptr");
            Error "void function does not return anything"
          in
          exn_ptr ++ young_ptr >>= fn
        end else begin
          let result = "%result" ^ c() in
          emit_instr (result ^ " = call " ^ calling_conv ^ " " ^ typename ret ^ " " ^ fn ^ "(" ^ args ^ ")");
          let exn_ptr = extractvalue ("%new_exn_ptr" ^ c()) ret result (Lconst("0", int_type)) in
          let young_ptr = extractvalue ("%new_young_ptr" ^ c()) ret result (Lconst("1", int_type)) in
          let call_res = extractvalue ("%call_res" ^ c()) ret result (Lconst("2", int_type)) in
          let fn ((exn_ptr, young_ptr), call_res) =
            emit_instr ("store " ^ typename addr_type ^ " " ^ exn_ptr ^ ", " ^ typename (Address addr_type) ^ "%exn_ptr");
            emit_instr ("store " ^ typename addr_type ^ " " ^ young_ptr ^ ", " ^ typename (Address addr_type) ^ "%young_ptr");
            Just call_res
          in
          exn_ptr ++ young_ptr ++ call_res >>= fn
        end
      in
      emit_llvm fn >>= f
  | Ltailcall(ret, fn, args) ->
      let args =
        let fn x =
          match emit_llvm x with
          | Just s -> typename addr_type ^ " " ^ s
          | Error s -> error ("failed to emit code for arguments of caml call:\n" ^ s)
        in
        List.map fn args
      in
      let args = String.concat ", "
        begin
          match emit_llvm exn_ptr, emit_llvm young_ptr with
          | Just e, Just y -> (typename addr_type ^ " " ^ e) :: (typename addr_type ^ " " ^ y) :: args
          | _, _ -> error ("failed to emit code for exn or young pointer")
        end
      in
      let f fn =
        print_endline "calling function...";
        if ret == Void then begin (* TODO even void functions should return caml_young_ptr and caml_exn_ptr *)
          emit_instr ("call " ^ calling_conv ^ " void " ^ fn ^ "(" ^ args ^ ")");
          emit_instr "ret void";
          Error "void function does not return anything"
        end else begin
          let c = c() in
          let result = "%result" ^ c in
          emit_instr (result ^ " = call " ^ calling_conv ^ " " ^ typename ret ^ " " ^ fn ^ "(" ^ args ^ ")");
          emit_instr ("ret " ^ typename Return3 ^ " " ^ result);
          Error "Tail call does not return anything for local usage"
        end
      in
      emit_llvm fn >>= f
  | Lccall(ret, fn, args) ->
      let args =
        let fn x =
          match emit_llvm x with
          | Just s -> typename (typeof x) ^ " " ^ s
          | Error s -> error ("failed to emit code for arguments of c call:\n" ^ s)
        in
        String.concat ", " (List.map fn args)
      in
      (* TODO store caml_young_ptr and caml_exn_ptr in the global variables *)
      (* TODO load caml_young_ptr and caml_exn_ptr into their registers *)
      let f fn =
        if ret == Void then begin
          emit_instr ("call ccc void " ^ fn ^ "(" ^ args ^ ")");
          Error "void c function does not return anything"
        end else begin
          let res = "%result" ^ c() in
          emit_instr (res ^ " = call ccc " ^ typename ret ^ " " ^ fn ^ "(" ^ args ^ ")");
          Just res
        end
      in
      emit_llvm fn >>= f
  | Lconst(const, _) -> Just const
  | Llabel name ->
      emit_label name;
      Error "label does not return anything"
  | Lbr label_name -> emit_instr ("br label %" ^ label_name); Error "br does not return anything"
  | Lbr_cond(cond, then_label, else_label) ->
      let typ = typename (typeof cond) in
      let fn cond =
        emit_instr ("br " ^ typ ^ " " ^ cond ^ ", label %" ^ then_label ^ ", label %" ^ else_label);
        Error "br_cond deos not return anything"
      in
      emit_llvm cond >>= fn
  | Lswitch(c, value, indexes, blocks, typ) ->
      let fn value =
        let f i =
          let i = string_of_int i in
          typename int_type ^ " " ^ i ^ ", label %label" ^ i ^ "." ^ c
        in
        emit_instr ("switch " ^ typename int_type ^ " " ^ value ^ ", label %default" ^ c ^ " [" ^
                 String.concat "\n" (List.map f indexes) ^ "]")
      in
      ignore (emit_llvm value >>= just fn);
      emit_llvm blocks
  | Lreturn(value, typ) -> emit_return value;
  | Lseq(instr1,Lcomment s) -> let res = emit_llvm instr1 in ignore (emit_llvm (Lcomment s)); res
  | Lseq(instr1,instr2) -> ignore (emit_llvm instr1); emit_llvm instr2

  | Lcaml_raise_exn exn ->
      let fn ((exn_ptr,young_ptr),exn) =
        let typ = typename addr_type in
        emit_instr ("call " ^ calling_conv ^ " void @caml_raise_exn(" ^ typ ^ " " ^ exn_ptr ^
                 ", " ^ typ ^ " " ^ young_ptr ^ ", " ^ typ ^ " " ^ exn ^ ") noreturn");
        emit_instr "unreachable";
        Error "raise does not return anything"
      in
      emit_llvm exn_ptr ++ emit_llvm young_ptr ++ emit_llvm exn >>= fn
  | Lcaml_catch_exn foo -> Just "CATCH"
  | Lcaml_alloc len ->
      let counter = c () in
      let new_young_ptr_instr =
        let offset = string_of_int (- len) in
        Lstore(Lgetelementptr(young_ptr, Lconst(offset, int_type)), Lalloca("new_young_ptr" ^ counter, addr_type))
      in
      let new_young_ptr = Lload(Lvar("%new_young_ptr" ^ counter, Address addr_type)) in
      let limit = Lload(Lvar("@caml_young_limit", Address addr_type)) in
      let cmp_res = Lcomp("icmp ult", addr_type, new_young_ptr, limit) in
      let result =
        Lbr ("begin_alloc" ^ counter)
        @@ Llabel ("begin_alloc" ^ counter)
        @@ Lcomment ("allocating " ^ string_of_int len ^ " bytes")
        @@ new_young_ptr_instr
        @@ Lbr_cond(cmp_res, "run_gc" ^ counter, "continue" ^ counter)
        @@ Llabel ("run_gc" ^ counter)
        @@ Lcall(Void, Lvar("@caml_call_gc", Any), [])
        @@ Lbr ("begin_alloc" ^ counter)
        @@ Llabel ("continue" ^ counter)
        @@ Lstore(new_young_ptr, Lvar("%young_ptr", Address addr_type)) in
      ignore (emit_llvm result);
      emit_llvm young_ptr

  | Ldefine(name, args, body) ->
      counter := 0;
      let args = String.concat ", " (List.map (fun (name, typ) -> typename typ ^ " " ^ name) args) in
      emit_nl ("define " ^ calling_conv ^ " %res @" ^ name ^ "(" ^ args ^ ") nounwind gc \"ocaml\" {");
      ignore (emit_llvm body);
      emit_nl "}\n";
      Error "define does not return anything"
  | Lnothing -> Error "nothing"
  | Lunreachable -> emit_instr "unreachable"; Error "unreachable does not return anything"
  | Lcomment s -> emit_instr ("; " ^ s); Error "comment does not return anything"

and emit_binop op typ left right =
  let fn (left,right) =
    let res = "%" ^ translate_symbol op ^ "_res" ^ c () in
    emit_nl ("\t" ^ res ^ " = " ^ op ^ " " ^ typ ^ " " ^ left ^ ", " ^ right);
    res
  in
  emit_llvm left ++ emit_llvm right >>= just fn

and emit_unop op typ arg =
  let fn arg =
    let res = "%" ^ translate_symbol op ^ "_res" ^ c () in
    emit_instr (res ^ " = " ^ op ^ " " ^ typ ^ " " ^ arg);
    res
  in
  emit_llvm arg >>= just fn

and cast value op from_type to_type =
  let fn value =
    let res = "%" ^ translate_symbol op ^ "_res" ^ c () in
    emit_instr (res ^ " = " ^ op ^ " " ^ (typename from_type) ^ " " ^ value ^ " to " ^ typename to_type);
    res
  in
  emit_llvm value >>= just fn

and emit_return value =
  let typ = typename (typeof value) in
  let fn ((value, exn_ptr), young_ptr) =
    emit_instr ("%mrv.0 = insertvalue %res undef, " ^ typ ^ " " ^ exn_ptr ^ ", 0");
    emit_instr ("%mrv.1 = insertvalue %res %mrv.0, " ^ typ ^ " " ^ young_ptr ^ ", 1");
    emit_instr ("%mrv.2 = insertvalue %res %mrv.1, " ^ typ ^ " " ^ value ^ ", 2");
    emit_instr ("ret %res  %mrv.2");
    Error "return statement does not write into any SSA register"
  in
  let value = emit_llvm value in
  let exn_ptr = emit_llvm exn_ptr in
  let young_ptr = emit_llvm young_ptr in
  value ++ exn_ptr ++ young_ptr >>= fn

and extractvalue res typ record index =
  let fn index =
    emit_instr (res ^ " = extractvalue " ^ typename typ ^ " " ^ record ^ ", " ^ index);
    Just res
  in
  emit_llvm index >>= fn

(*
 * Header with declarations of some functions and constants needed in every
 * module.
 *)
let header =
  [ "; vim: set ft=llvm:"
  ; "%res = type " ^ ret_type
  ; "%res2 = type {" ^ typename addr_type ^ ", " ^ typename addr_type ^ "}"
  ; "declare double @fabs(double)"
  ; "declare void @caml_raise_exn(i64*, i64*, i64*) noreturn"
  ; "declare %res2 @caml_call_gc(i64*, i64*)"
  ; "@caml_exception_pointer = external global i64*"
  ; "@caml_young_ptr = external global i64*"
  ; "@caml_young_limit = external global i64*"
  ; "@caml_bottom_of_stack = external global i64*"
  ; "@caml_last_return_address  = external global i64*"
  ]

let constants : string list ref = ref []

let functions : (string * string * string list) list ref = ref []

let module_asm () = emit_string "module asm \""

let add_const str =
  if List.exists (fun x -> String.compare x str == 0) !constants
  then ()
  else constants := str :: !constants

let add_function (ret, str, args) =
  if List.exists (fun (_, x,_) -> String.compare x str == 0) !functions
  then ()
  else functions := (typename ret, str, List.map (fun _ -> "i" ^ string_of_int size_int ^ "*") args) :: !functions

let emit_function_declarations () =
  List.iter (fun (ret_type, name, args) -> emit_nl ("declare " ^ calling_conv ^ " " ^ ret_type ^ " @" ^ name ^
                                     "(" ^ String.concat "," args ^ ")")) !functions

let emit_constant_declarations () =
  List.iter (fun name -> emit_nl ("@" ^ name ^ " = external global " ^ typename int_type)) !constants

(* Print an expression in the intermediate format using a syntax inspired by
 * S-expressions *)
let rec to_string = function
  | Lvar(name, typ) -> "(local_store " ^ typename typ ^ " " ^ name ^ ")"
  | Lbinop(op, typ, left, right) -> "(" ^ op ^ " " ^ typename typ ^ " " ^ to_string left ^ " " ^ to_string right ^ ")"
  | Lcomp(op, typ, left, right) -> "(" ^ op ^ " " ^ typename typ ^ " " ^ to_string left ^ " " ^ to_string right ^ ")"
  | Lunop(op, typ, arg) -> "(" ^ op ^ " " ^ typename typ ^ " " ^ to_string arg ^ ")"
  | Lalloca(name, typ) -> "(alloca " ^ name ^ " " ^ typename typ ^ ")"
  | Lload addr -> "(load " ^ to_string addr ^ ")"
  | Lstore(value, addr) -> "(store " ^ to_string value ^ " " ^ to_string addr ^ ")"
  | Lzext(value, from_type, to_type)     -> "(zext"     ^ to_string value ^ " " ^ typename from_type ^ " " ^ typename to_type ^ ")"
  | Ltrunc(value, from_type, to_type)    -> "(trunc"    ^ to_string value ^ " " ^ typename from_type ^ " " ^ typename to_type ^ ")"
  | Lbitcast(value, from_type, to_type)  -> "(bitcast"  ^ to_string value ^ " " ^ typename from_type ^ " " ^ typename to_type ^ ")"
  | Linttoptr(value, from_type, to_type) -> "(inttoptr" ^ to_string value ^ " " ^ typename from_type ^ " " ^ typename to_type ^ ")"
  | Lptrtoint(value, from_type, to_type) -> "(ptrtoint" ^ to_string value ^ " " ^ typename from_type ^ " " ^ typename to_type ^ ")"
  | Lsitofp(value, from_type, to_type)   -> "(sitofp"   ^ to_string value ^ " " ^ typename from_type ^ " " ^ typename to_type ^ ")"
  | Lfptosi(value, from_type, to_type)   -> "(fptosi"   ^ to_string value ^ " " ^ typename from_type ^ " " ^ typename to_type ^ ")"
  | Lgetelementptr(addr, offset) -> "(elemptr " ^ to_string addr ^ " " ^ to_string offset ^ ")"
  | Lcall(ret, fn, args) -> "(call " ^ typename ret ^ " " ^ to_string fn ^ "(" ^ String.concat " " (List.map to_string args) ^ "))"
  | Ltailcall(ret, fn, args) -> "(tailcall " ^ typename ret ^ " " ^ to_string fn ^ "(" ^ String.concat " " (List.map to_string args) ^ "))"
  | Lccall(ret, fn, args) -> "(call ccc " ^ typename ret ^ " " ^ to_string fn ^ "(" ^ String.concat " " (List.map to_string args) ^ "))"
  | Lconst(const, typ) -> "(const " ^ const ^ " " ^ typename typ ^ ")"
  | Llabel name -> "(label " ^ name ^ ")"
  | Lbr label_name -> "(br " ^ label_name ^ ")"
  | Lbr_cond(cond, then_label, else_label) -> "(if " ^ to_string cond ^ " (br " ^ then_label ^ ") (br " ^ else_label ^ "))"
  | Lswitch(c, value, indexes, blocks, typ) -> "(switch " ^ typename typ ^ " " ^ to_string value ^ " of " ^ to_string blocks ^ ")"
  | Lreturn(value, typ) -> "(return " ^ typename typ ^ " " ^ to_string value ^ ")"
  | Lseq(instr1,instr2) -> to_string instr1 ^ ";\n\t" ^ to_string instr2
  | Lcaml_raise_exn exn -> "(raise " ^ to_string exn ^ ")"
  | Lcaml_catch_exn foo -> "(catch " ^ foo ^ ")"
  | Lcaml_alloc len -> "(ALLOC " ^ string_of_int len ^ ")"
  | Ldefine(name, args, body) -> "(define " ^ name ^ " " ^ String.concat " " (List.map (fun (x,_) -> x) args) ^ " " ^ to_string body ^ ")"
  | Lnothing -> "nothing"
  | Lunreachable -> "unreachable"
  | Lcomment s -> "(comment " ^ s ^ ")"



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
  emit_constant_declarations ()


let run_assembler asm infile outfile =
  Ccomp.command (asm ^ " -o " ^ Filename.quote outfile ^ " " ^ Filename.quote infile)

let assemble_file temp1 temp2 infile outfile =
  ignore (Ccomp.command ("cp " ^ infile ^ " " ^ infile ^ ".ll"));
  let res = run_assembler Config.opt infile temp1 in 
  if res <> 0 then res else
  let res = run_assembler Config.llc temp1 temp2 in 
  if res <> 0 then res else
    run_assembler Config.asm temp2 outfile

(* vim: set foldenable : *)
