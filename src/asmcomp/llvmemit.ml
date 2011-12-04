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

type llvm_type =
  | Integer of int (* bitwidth *)
  | Double
  | Address of llvm_type
  | Return_type
  | Void
  | Any
  | Function of llvm_type * llvm_type list (* return type, argument types *)

let int_type = Integer size_int
let addr_type = Address int_type
let float_sized_int = Integer size_float

let counter_inc () = counter := !counter + 1
let c () = counter_inc (); "." ^ string_of_int !counter

let ret_type = "{i" ^ string_of_int size_int ^ "*, i" ^ string_of_int size_int ^ "*, i" ^ string_of_int size_int ^ "*}"

let rec typename = function
  | Integer i -> "i" ^ string_of_int i
  | Double -> "double"
  | Address typ -> typename typ ^ "*"
  | Return_type -> ret_type
  | Void -> "void"
  | Any -> (*error "unable to infer type"*) "any"
  | Function(ret, args) -> typename ret ^ " (" ^ String.concat ", " (List.map typename args) ^ ")"

let deref typ = match typ with
  | Address typ -> typ
  | _ -> error ("trying to dereference non-pointer type " ^ typename typ)

type llvm_instr =
  | Llocal_load of string * llvm_type (* name, type *)
  | Llocal_store of string * llvm_type (* name, type *)
  | Lalias of string * llvm_instr (* name, value *)
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
  | Llocal_load(_, typ) -> deref typ
  | Llocal_store(_, typ) -> typ
  | Lalias(_, value) -> typeof value
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
  | Lcall(typ,_,_) -> if typ == Return_type then addr_type else typ
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
  | Ldefine(_,_,_) -> Return_type
  | Lnothing -> error "Lnothing does not have a type"
  | Lunreachable -> error "Lunreachable does not have a type"
  | Lcomment _ -> error "Lcomment does not have a type"

let (@@) a b = Lseq(a, b)

let types = Hashtbl.create 10

let llvm_instrs : llvm_instr list ref = ref []

let (>>=) value fn = match value with
  | Just value -> fn value
  | Error s -> Error s

let just fn = fun x -> Just (fn x)

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

let rec emit_llvm instr =
  let (++) a b = match a with Just a -> begin match b with Just b -> Just (a,b) | Error e-> Error e end | Error e -> Error e in
  let emit_op op typ arg =
    let res = "%" ^ translate_symbol op ^ "_res" ^ c () in
    emit_nl ("\t" ^ res ^ " = " ^ op ^ " " ^ typ ^ " " ^ arg); res in
  let cast value op from_type to_type =
    let fn value = emit_op op (typename from_type) (value ^ " to " ^ typename to_type) in
    emit_llvm value >>= just fn in
  match instr with
  | Llocal_load(name, typ) -> let res = "%local_load_res" ^ c () in emit_nl ("\t" ^ res ^ " = load " ^ typename typ ^ " " ^  name); Just res
  | Llocal_store(name, typ) -> Just name
  | Lalias(name, Lptrtoint(value, src_type, dest_type)) ->
      let fn value =
        emit_nl ("\t" ^ name ^ " = ptrtoint " ^ typename src_type ^ " " ^ value ^ " to " ^ typename dest_type); name in
      emit_llvm value >>= just fn
  | Lalias(name, Lgetelementptr(addr, offset)) ->
      let offset_type = typename (typeof offset) in
      let addr_type = typename (typeof addr) in
      let fn (addr,offset) = emit_nl ("\t" ^ name ^ " = getelementptr " ^ addr_type ^ " " ^ addr ^ ", " ^ offset_type ^ " " ^ offset); name in
      emit_llvm addr ++ emit_llvm offset >>= just fn
  | Lalias(name, _) -> error "not implemented yet"
  | Lbinop(op, typ, left, right) ->
      let fn (left,right) = emit_op op (typename typ) (left ^ ", " ^ right) in
      emit_llvm left ++ emit_llvm right >>= just fn
  | Lcomp(op, typ, left, right) ->
      let fn (left,right) = emit_op op (typename typ) (left ^ ", " ^ right) in
      emit_llvm left ++ emit_llvm right >>= just fn
  | Lunop(op, typ, arg) ->
      let fn arg = emit_op op (typename typ) arg in
      emit_llvm arg >>= just fn
  | Lalloca(name, typ) -> emit_nl ("\t%" ^ name ^ " = alloca " ^ typename typ); Just ("%" ^ name)
  | Lload addr -> emit_llvm addr >>= just (emit_op "load" (typename (typeof addr)))
  | Lstore(value, addr) ->
      let fn (v,a) =
        emit_nl ("\tstore " ^ typename (typeof value) ^ " " ^ v ^ ", " ^ typename (typeof addr) ^ " " ^ a);
        Error "store does not return anything"
      in
      emit_llvm value ++ emit_llvm addr >>= fn
  | Lzext(value, from_type, to_type)     -> cast value "zext"     from_type to_type
  | Ltrunc(value, from_type, to_type)    -> cast value "trunc"    from_type to_type
  | Lbitcast(value, from_type, to_type)  -> cast value "bitcast"  from_type to_type
  | Linttoptr(value, from_type, to_type) -> cast value "inttoptr" from_type to_type
  | Lptrtoint(value, from_type, to_type) -> cast value "ptrtoint" from_type to_type
  | Lsitofp(value, from_type, to_type)   -> cast value "sitofp"   from_type to_type
  | Lfptosi(value, from_type, to_type)   -> cast value "fptosi"   from_type to_type
  | Lgetelementptr(addr, offset) -> emit_llvm (Lalias("%elemptr" ^ c(), instr))
  | Lcall(ret, fn, args) ->
      let args =
        String.concat ", " (List.map (fun x -> match emit_llvm x with
                                               | Just s -> typename addr_type ^ " " ^ s
                                               | Error s -> error ("failed to emit code for arguments of caml call:\n" ^ s)) args) in
      if ret == Void
      then
        let f fn = emit_nl ("\tcall cc 11 void " ^ fn ^ "(" ^ args ^ ")"); Error "void function does not return anything" in
        emit_llvm fn >>= f
      else
        let f fn =
          let c = c() in
          let result = "%result" ^ c in
          emit_nl ("\t" ^ result ^ " = call cc 11 " ^ typename ret ^ " " ^ fn ^ "(" ^ args ^ ")");
          let exn_ptr = "%new_exn_ptr" ^ c in
          emit_nl ("\t" ^ exn_ptr ^ " = extractvalue " ^ typename ret ^ " " ^ result ^ ", 1");
          emit_nl ("\tstore " ^ typename addr_type ^ " " ^ exn_ptr ^ ", " ^ typename (Address addr_type) ^ "%exn_ptr");
          let young_ptr = "%new_young_ptr" ^ c in
          emit_nl ("\t" ^ young_ptr ^ " = extractvalue " ^ typename ret ^ " " ^ result ^ ", 2");
          emit_nl ("\tstore " ^ typename addr_type ^ " " ^ young_ptr ^ ", " ^ typename (Address addr_type) ^ "%young_ptr");
          let call_res = "%call_res" ^ c in
          emit_nl ("\t" ^ call_res ^ " = extractvalue " ^ typename ret ^ " " ^ result ^ ", 0");
          Just call_res
        in emit_llvm fn >>= f
  | Lccall(ret, fn, args) ->
      let args =
        String.concat ", " (List.map (fun x -> match emit_llvm x with
                                               | Just s -> typename (typeof x) ^ " " ^ s
                                               | Error s -> error ("failed to emit code for arguments of c call:\n" ^ s)) args) in
      if ret == Void
      then
        let f fn = emit_nl ("\tcall ccc void " ^ fn ^ "(" ^ args ^ ")"); Error "void c function does not return anything" in
        emit_llvm fn >>= f
      else
        let f fn =
          let res = "%result" ^ c() in
          emit_nl ("\t" ^ res ^ " = call ccc " ^ typename ret ^ " " ^ fn ^ "(" ^ args ^ ")");
          Just res
        in emit_llvm fn >>= f
  | Lconst(const, _) -> Just const
  | Llabel name -> emit_nl (name ^ ":"); Error "label does not return anything"
  | Lbr label_name -> emit_nl ("\tbr label %" ^ label_name); Error "br does not return anything"
  | Lbr_cond(cond, then_label, else_label) ->
      let typ = typename (typeof cond) in
      let fn cond =
        emit_nl ("\tbr " ^ typ ^ " " ^ cond ^ ", label %" ^ then_label ^ ", label %" ^ else_label);
        Error "br_cond deos not return anything"
      in emit_llvm cond >>= fn
  | Lswitch(c, value, indexes, blocks, typ) ->
      let fn value =
        emit_nl ("\tswitch " ^ typename int_type ^ " " ^ value ^ ", label %default" ^ c ^ " [" ^
                     String.concat "\n"
                       (List.map (fun i -> let i = string_of_int i in typename int_type ^ " " ^ i ^ ", label %label"^i^"."^c) indexes)
                     ^ "]")
      in
      ignore (emit_llvm value >>= just fn);
      emit_llvm blocks
  | Lreturn(value, typ) ->
      let typ = typename addr_type in
      let res = emit_llvm value in
      let exn_ptr = emit_llvm (Llocal_load ("%exn_ptr", Address addr_type)) in
      let young_ptr = emit_llvm (Llocal_load ("%young_ptr", Address addr_type)) in
      let fn ((exn_ptr,young_ptr),res) =
        emit_nl ("\tret " ^ typ ^ " " ^ res ^ ", " ^ typ ^ " " ^ exn_ptr ^ ", " ^ typ ^ " " ^ young_ptr);
        Error "return statement does not return anything"
      in exn_ptr ++ young_ptr ++ res >>= fn
  | Lseq(instr1,Lcomment s) -> let res = emit_llvm instr1 in ignore (emit_llvm (Lcomment s)); res
  | Lseq(instr1,instr2) -> ignore (emit_llvm instr1); emit_llvm instr2

  | Lcaml_raise_exn exn ->
      let fn ((exn_ptr,young_ptr),exn) =
        let typ = typename addr_type in
        emit_nl ("\tcall cc 11 void @caml_raise_exn(" ^ typ ^ " " ^ exn_ptr ^ ", " ^ typ ^ " " ^ young_ptr ^ ", " ^ typ ^ " " ^ exn ^ ") noreturn");
        emit_nl "\tunreachable";
        Error "raise does not return anything"
      in
      let exn_ptr = emit_llvm (Llocal_load ("%exn_ptr", Address addr_type)) in
      let young_ptr = emit_llvm (Llocal_load ("%young_ptr", Address addr_type)) in
      exn_ptr ++ young_ptr ++ emit_llvm exn >>= fn
  | Lcaml_catch_exn foo -> Just "CATCH"
  | Lcaml_alloc len ->
      emit_nl ("\t; allocating " ^ string_of_int len ^ " bytes");
      let counter = c () in
      let new_young_ptr_instr =
        let offset = string_of_int (- len) in
        let young_ptr = Llocal_load ("%young_ptr", Address addr_type) in
        Lstore(Lgetelementptr(young_ptr, Lconst(offset, int_type)), Lalloca("new_young_ptr" ^ counter, addr_type))
      in
      let new_young_ptr = Llocal_load("%new_young_ptr" ^ counter, Address addr_type) in
      let limit = Llocal_load("@caml_young_limit", Address addr_type) in
      let cmp_res = Lcomp("icmp ult", addr_type, new_young_ptr, limit) in
      let result =
        Lbr ("begin_alloc" ^ counter)
        @@ Llabel ("begin_alloc" ^ counter)
        @@ Lcomment ("allocating " ^ string_of_int len ^ " bytes")
        @@ new_young_ptr_instr
        @@ Lbr_cond(cmp_res, "run_gc" ^ counter, "continue" ^ counter)
        @@ Llabel ("run_gc" ^ counter)
        @@ Lcall(Void, Llocal_store("@caml_call_gc", Any),
                 [Llocal_load("%exn_ptr", Address addr_type); Llocal_load("%young_ptr", Address addr_type)])
        @@ Lbr ("begin_alloc" ^ counter)
        @@ Llabel ("continue" ^ counter)
        @@ Lstore(new_young_ptr, Llocal_store("%young_ptr", Address addr_type)) in
      ignore (emit_llvm result);
      emit_llvm new_young_ptr

  | Ldefine(name, args, body) ->
      counter := 0;
      let args = String.concat ", " (List.map (fun (name, typ) -> typename typ ^ " " ^ name) args) in
      emit_nl ("define cc 11 %res @" ^ name ^ "(" ^ args ^ ") nounwind gc \"ocaml\" {");
      ignore (emit_llvm body >>= just emit_nl);
      emit_nl "}\n";
      Error "define does not return anything"
  | Lnothing -> Just "1"
  | Lunreachable -> emit_nl "\tunreachable"; Error "unreachable does not return anything"
  | Lcomment s -> emit_nl ("; " ^ s); Error "comment does not return anything"

let header = [ "; vim: set ft=llvm:"
             ; "%res = type " ^ ret_type
             ; "declare double @fabs(double)"
             ; "declare void @caml_raise_exn(i64*, i64*, i64*) noreturn"
             ; "declare void @caml_call_gc(i64*, i64*)"
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
  List.iter (fun (ret_type, name, args) -> emit_nl ("declare cc 11 " ^ ret_type ^ " @" ^ name ^
                                     "(" ^ String.concat "," args ^ ") nounwind")) !functions

let emit_constant_declarations () =
  List.iter (fun name -> emit_nl ("@" ^ name ^ " = external global " ^ typename int_type)) !constants

let rec to_string = function
  | Llocal_load(name, typ) -> "(local_load " ^ typename typ ^ " " ^ name ^ ")"
  | Llocal_store(name, typ) -> "(local_store " ^ typename typ ^ " " ^ name ^ ")"
  | Lalias(name, foo) -> "(alias " ^ name ^ " " ^ to_string foo ^ ")"
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

let emit_label l =
  emit_string (".L" ^ string_of_int l)

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


let emit_item = function
    Cglobal_symbol s ->
      (module_asm(); emit_string "\t.globl  "; emit_symbol s; emit_string "\"\n");
  | Cdefine_symbol s ->
      (module_asm(); emit_symbol s; emit_string ":\"\n")
  | Cdefine_label lbl ->
      (module_asm(); emit_label (100000 + lbl); emit_string ":\"\n")
  | Cint8 n ->
      (module_asm(); emit_string "\t.byte   "; emit_int n; emit_string "\"\n")
  | Cint16 n ->
      (module_asm(); emit_string "\t.word   "; emit_int n; emit_string "\"\n")
  | Cint32 n ->
      (module_asm(); emit_string "\t.long   "; emit_nativeint n; emit_string "\"\n")
  | Cint n ->
      (module_asm(); emit_string "\t.quad   "; emit_nativeint n; emit_string "\"\n")
  | Csingle f ->
      module_asm(); emit_float32_directive ".long" f; emit_string "\""
  | Cdouble f ->
      module_asm(); emit_float64_directive ".quad" f; emit_string "\""
  | Csymbol_address s ->
      (module_asm(); emit_string "\t.quad   "; emit_symbol s; emit_string "\"\n")
  | Clabel_address lbl ->
      (module_asm(); emit_string "\t.quad   "; emit_label (100000 + lbl); emit_string "\"\n")
  | Cstring s ->
      emit_string_directive "\t.ascii  " s
  | Cskip n ->
      if n > 0 then (module_asm(); emit_string "\t.space  "; emit_int n; emit_string "\"\n")
  | Calign n ->
      module_asm(); emit_align n

let data l =
  module_asm(); emit_nl "\t.data\"";
  List.iter emit_item l

let begin_assembly() = List.iter emit_nl header

let end_assembly() = ()


let assemble_file infile tempfile outfile =
  let res =
    Ccomp.command ("echo_and_run opt -S -std-compile-opts -o " ^
                   Filename.quote tempfile ^ " " ^ Filename.quote infile)
  in 
  if res <> 0 then res else
  let res =
    Ccomp.command (Config.llc ^ " -o " ^
                   Filename.quote (tempfile ^ "2") ^ " " ^ Filename.quote infile)
  in 
  if res <> 0 then res else
    Ccomp.command (Config.asm ^ " -o " ^
                   Filename.quote outfile ^ " " ^ Filename.quote (tempfile ^ "2"))

(* vim: set foldenable : *)
