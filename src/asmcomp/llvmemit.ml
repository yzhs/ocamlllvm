open Cmm
open Emitaux

open Llvm

exception Llvm_error of string

type 'a error = Just of 'a | Error of string

let error str = raise (Llvm_error str)

let emit_string s = emit_string (s ^ "\n")

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
  | Function(ret, args) -> typename ret ^ " (" ^ String.concat ", " (List.map typename args) ^ ")*"

let deref typ = match typ with
  | Address typ -> typ
  | _ -> error ("trying to dereference non-pointer type " ^ typename typ)

type llvm_instr =
  | Lvar of string * llvm_type (* name, type *)
  | Lbinop of string * llvm_type * llvm_instr * llvm_instr (* op, typ, left, right *)
  | Lcomp of string * llvm_type * llvm_instr * llvm_instr (* op, typ, left, right *)
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
  | Lbr of string (* label *)
  | Lbr_cond of llvm_instr * string * string (* condition, then label, else label *)
  | Lseq of llvm_instr * llvm_instr (* value, type *)
  | Lblock of string * llvm_instr (* name, block instructions *)
  | Lbegin_block of string (* name *)
  | Lifthenelse of llvm_instr * llvm_instr * llvm_instr * llvm_instr (* cond, then, else, result *)
  | Lswitch of string * llvm_instr * int list * llvm_instr list * llvm_type (* counter, expr, indexes, blocks, typ *)
  | Lcase of string * llvm_instr (* label name, instructions *)
  | Lreturn of llvm_instr * llvm_type (* value, type *)
  | Lcaml_raise_exn of llvm_instr (* argument *)
  | Lcaml_catch_exn of string (* TODO figure out what information is needed *)
  | Lcaml_alloc of int (* length *)
  | Ldefine of string * (string * llvm_type) list * llvm_instr (* name, arguments, body *)
  | Lnothing
  | Lunreachable

let rec typeof = function
  | Lvar(_, typ) -> typ
  | Lbinop(_,typ,_,_) -> typ
  | Lcomp(_,_,_,_) -> Integer 1
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
  | Lbr _ -> Void
  | Lbr_cond(_,_,_) -> Void
  | Lseq(instr ,Lbr _) -> typeof instr
  | Lseq(instr ,Lbr_cond(_,_,_)) -> typeof instr
  | Lseq(_,instr) -> typeof instr
  | Lblock(_,instrs) -> typeof instrs
  | Lbegin_block _ -> error "label does not have a type"
  | Lifthenelse(_,_,_,res) -> typeof res
  | Lswitch(_,_,_,_,typ) -> typ
  | Lcase(_,instr) -> typeof instr
  | Lreturn(_, typ) -> typ
  | Lcaml_raise_exn _ -> Void
  | Lcaml_catch_exn _ -> Void
  | Lcaml_alloc _ -> addr_type
  | Ldefine(_,_,_) -> Return_type
  | Lnothing -> error "Lnothing does not have a type"
  | Lunreachable -> error "Lunreachable does not have a type"

let (@@) a b = Lseq(a, b)

let translate_symbol s =
  let result = ref "" in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
          result := !result ^ Printf.sprintf "%c" c
    | _ -> result := !result ^ Printf.sprintf "$%02x" (Char.code c)
  done;
  !result

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

let context = global_context ()
let the_module = create_module context "OCaml LLVM jit"
let builder = builder context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let rec translate = function
  | Integer i -> integer_type context i
  | Address typ -> pointer_type (translate typ)
  | Double -> double_type
  | Void -> void_type context
  | Any -> error "unknown type"
  | Return_type -> struct_type context [|translate addr_type; translate addr_type; translate addr_type|]
  | Function(ret, args) -> function_type (translate ret) (Array.of_list (List.map translate args))

let rec emit_llvm instr =
  let emit_binop op typ left right =
    (match op with
      | "add"  -> build_add
      | "sub"  -> build_sub
      | "mul"  -> build_mul
      | "sdiv" -> build_sdiv
      | "srem" -> build_srem
      | "and"  -> build_and
      | "or"   -> build_or
      | "xor"  -> build_xor
      | "shl"  -> build_shl
      | "lshr" -> build_lshr
      | "ashr" -> build_ashr
      | "fadd" -> build_fadd
      | "fsub" -> build_fsub
      | "fmul" -> build_fmul
      | "fdiv" -> build_fdiv
      | "fcmp oeq" -> build_fcmp Fcmp.Oeq
      | "fcmp one" -> build_fcmp Fcmp.One
      | "fcmp olt" -> build_fcmp Fcmp.Olt
      | "fcmp ole" -> build_fcmp Fcmp.Ole
      | "fcmp ogt" -> build_fcmp Fcmp.Ogt
      | "fcmp oge" -> build_fcmp Fcmp.Oge
      | "icmp eq"  -> build_icmp Icmp.Eq
      | "icmp ne"  -> build_icmp Icmp.Ne
      | "icmp slt" -> build_icmp Icmp.Slt
      | "icmp sle" -> build_icmp Icmp.Sle
      | "icmp sgt" -> build_icmp Icmp.Sgt
      | "icmp sge" -> build_icmp Icmp.Sge
      | "icmp ult" -> build_icmp Icmp.Ult
      | "icmp ule" -> build_icmp Icmp.Ule
      | "icmp ugt" -> build_icmp Icmp.Ugt
      | "icmp uge" -> build_icmp Icmp.Uge
      | _ -> error "unknown operator") left right "op_res" builder in
  match instr with
  | Lvar(name, typ) -> Printf.printf "var %s...\n" name; flush stdout; const_int (i64_type context) 1 (* TODO create llvalue from this name... *)

  | Lbinop(op, typ, left, right) -> Printf.printf "binop...\n"; flush stdout;emit_binop op typ (emit_llvm left) (emit_llvm right)
  | Lcomp(op, typ, left, right) -> Printf.printf "comparing...\n"; flush stdout; emit_binop op typ (emit_llvm left) (emit_llvm right)
  | Lalloca(name, typ) -> Printf.printf "alloca...\n"; flush stdout; build_alloca (translate typ) name builder
  | Lload addr -> Printf.printf "loading...\n"; flush stdout; build_load (emit_llvm addr) "load" builder
  | Lstore(value, addr) -> Printf.printf "store...\n"; flush stdout; build_store (emit_llvm value) (emit_llvm addr) builder
  | Lzext    (value, _, to_type) -> Printf.printf "zext...\n"         ; flush stdout; build_zext     (emit_llvm value) (translate to_type) "zext"     builder
  | Ltrunc   (value, _, to_type) -> Printf.printf "trunc...\n"        ; flush stdout; build_trunc    (emit_llvm value) (translate to_type) "trunc"    builder
  | Lbitcast (value, _, to_type) -> Printf.printf "bitcast...\n"      ; flush stdout; build_bitcast  (emit_llvm value) (translate to_type) "bitcast"  builder
  | Linttoptr(value, _, to_type) -> Printf.printf "inttoptr...\n"     ; flush stdout; build_inttoptr (emit_llvm value) (translate to_type) "inttoptr" builder
  | Lptrtoint(value, _, to_type) -> Printf.printf "ptrtoint...\n"     ; flush stdout; build_ptrtoint (emit_llvm value) (translate to_type) "ptrtoint" builder
  | Lsitofp  (value, _, to_type) -> Printf.printf "sitofp...\n"       ; flush stdout; build_sitofp   (emit_llvm value) (translate to_type) "sitofp"   builder
  | Lfptosi  (value, _, to_type) -> Printf.printf "fptosi...\n"       ; flush stdout; build_fptosi   (emit_llvm value) (translate to_type) "fitosp"   builder
  | Lgetelementptr(addr, offset) -> Printf.printf "getelementptr...\n"; flush stdout; build_gep (emit_llvm addr) [|emit_llvm offset|] "elemptr" builder
  | Lcall(ret, fn, args) ->
      Printf.printf "calling ocaml function %s\n" (to_string fn); flush stdout;
      if ret == Void
      then
        build_call (emit_llvm fn) (Array.of_list (List.map emit_llvm args)) "void" builder
      else begin
        let call = build_call (emit_llvm fn) (Array.of_list (List.map emit_llvm args)) "call" builder in
        let young_ptr = build_extractelement call (const_int (translate int_type) 1) "new_young_ptr" builder in
        let exn_ptr = build_extractelement call (const_int (translate int_type) 2) "new_exn_ptr" builder in
        ignore (build_store young_ptr (emit_llvm (Lvar("%young_ptr", Address addr_type))) builder);
        ignore (build_store exn_ptr (emit_llvm (Lvar("%exn_ptr", Address addr_type))) builder);
        build_extractelement call (const_int (translate int_type) 0) "call_res" builder
      end
  | Lccall(ret, fn, args) ->
      Printf.printf "calling c function %s\n" (to_string fn); flush stdout;
      build_call (emit_llvm fn) (Array.of_list (List.map emit_llvm args)) "extcall" builder
  | Lconst(const, typ) -> begin
      Printf.printf "constant...\n"; flush stdout;
      match typ with
      | Integer _ -> const_int_of_string (translate typ) const 10
      | Double -> const_float double_type (float_of_string const)
      | _ -> error "not implemented: constants that are neither integers nor floats"
    end
  | Lbr label_name -> error "branching not implemented"
  | Lbr_cond(cond, then_label, else_label) -> error "conditional branching not implemented"
  | Lblock(name, instrs) ->
      Printf.printf "generating block %s\n" name; flush stdout;
      let bb = append_block context name (block_parent (insertion_block builder)) in
      position_at_end bb builder;
      emit_llvm instrs
  | Lbegin_block name ->
      Printf.printf "starting block %s\n" name; flush stdout;
      let bb = append_block context name (block_parent (insertion_block builder)) in
      let res = build_br bb builder in
      position_at_end bb builder;
      res
  | Lifthenelse(cond, then_instr, else_instr, res) -> error "if not implemented"
  | Lswitch(c, value, indexes, blocks, typ) -> error "switch is not implemented"
  | Lcase(name, instr) -> error "case not implemented"
  | Lreturn(value, _) ->
      Printf.printf "returning value...\n"; flush stdout;
      let exn_ptr = emit_llvm (Lload (Lvar("%exn_ptr", Address addr_type))) in
      let young_ptr = emit_llvm (Lload (Lvar("%young_ptr", Address addr_type))) in
      let res = build_insertvalue (undef (translate Return_type)) (emit_llvm value) 0 "res_tmp" builder in
      let res = build_insertvalue res exn_ptr 1 "res_tmp" builder in
      let res = build_insertvalue res young_ptr 2 "res" builder in
      build_ret res builder
  | Lseq(Lnothing,instr) -> emit_llvm instr
  | Lseq(instr1,instr2) -> ignore (emit_llvm instr1); emit_llvm instr2

  | Lcaml_raise_exn exn ->
      Printf.printf "raising exception\n"; flush stdout;
      let args = [|emit_llvm (Lload (Lvar("%exn_ptr", Address addr_type)));
                   emit_llvm (Lload (Lvar("%young_ptr", Address addr_type)));
                   emit_llvm exn|]
      in
      ignore (build_call (emit_llvm (Lvar("@caml_raise_exn", Function(Void,[addr_type;addr_type;addr_type])))) args "raise" builder);
      build_unreachable builder
  | Lcaml_catch_exn foo -> error "catching exceptions not implemented"
  | Lcaml_alloc len ->
      Printf.printf "allocating...\n"; flush stdout;
      let young_ptr = emit_llvm (Lload (Lvar("%young_ptr", Address addr_type))) in
      let new_young_ptr = build_gep young_ptr [|const_int (translate int_type) (- len)|] "new_young_ptr" builder in
      let limit = emit_llvm (Lload(Lvar("@caml_young_limit", Address addr_type))) in
      let cmp_res = build_icmp Icmp.Ult new_young_ptr limit "cmp_res" builder in
      let bb = append_block context "begin_alloc" (block_parent (insertion_block builder)) in
      ignore(build_br bb builder);
      position_at_end bb builder;
      let run_gc = append_block context "run_gc" (block_parent bb) in
      let continue = append_block context "continue" (block_parent run_gc) in
      ignore (build_cond_br cmp_res run_gc continue builder);
      position_at_end run_gc builder;
      ignore (build_call (emit_llvm (Lvar("@caml_call_gc", Function(Void, [])))) (Array.map emit_llvm [|Lload(Lvar("%exn_ptr", Address addr_type)); Lload(Lvar("%young_ptr", Address addr_type))|]) "gc_res" builder);
      ignore (build_br bb builder);
      position_at_end continue builder;
      build_store new_young_ptr (emit_llvm (Lvar("%young_ptr", Address addr_type))) builder

  | Ldefine(name, args, body) ->
      Printf.printf "defining function %s ...\n" name; flush stdout;
      let typ = function_type (translate Return_type) (Array.make (List.length args) (translate addr_type)) in
      let fn = define_function name typ the_module in
      position_at_end (entry_block fn) builder;
      ignore (emit_llvm body);
      fn
  | Lnothing -> Printf.printf "nothing to emit"; flush stdout;
                build_add (const_int (translate int_type) 23) (const_int (translate int_type) 42) "nop" builder
  | Lunreachable ->
      Printf.printf "unreachable...\n"; flush stdout;
      build_unreachable builder

and to_string = function
  | Lvar(name, typ) -> "(local_store " ^ typename typ ^ " " ^ name ^ ")"
  | Lifthenelse(cond,then_instr,else_instr,res) ->
      "(if " ^ to_string cond ^ " " ^ to_string then_instr ^ " else " ^ to_string else_instr ^ " " ^ to_string res ^ ")"
  | Lbinop(op, typ, left, right) -> "(" ^ op ^ " " ^ typename typ ^ " " ^ to_string left ^ " " ^ to_string right ^ ")"
  | Lcomp(op, typ, left, right) -> "(" ^ op ^ " " ^ typename typ ^ " " ^ to_string left ^ " " ^ to_string right ^ ")"
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
  | Lcase(name, instr) -> "(case " ^ name ^ " " ^ to_string instr ^ ")"
  | Lblock(name, instrs) -> "(block " ^ name ^ " " ^ to_string instrs ^ ")"
  | Lbegin_block name -> "(label " ^ name ^ ")"
  | Lbr label_name -> "(br " ^ label_name ^ ")"
  | Lbr_cond(cond, then_label, else_label) -> "(if " ^ to_string cond ^ " (br " ^ then_label ^ ") (br " ^ else_label ^ "))"
  | Lswitch(c, value, indexes, blocks, typ) -> "(switch " ^ typename typ ^ " " ^ to_string value ^ " of " ^ String.concat "; " (List.map to_string blocks) ^ ")"
  | Lreturn(value, typ) -> "(return " ^ typename typ ^ " " ^ to_string value ^ ")"
  | Lseq(instr1,instr2) -> to_string instr1 ^ ";\n\t" ^ to_string instr2
  | Lcaml_raise_exn exn -> "(raise " ^ to_string exn ^ ")"
  | Lcaml_catch_exn foo -> "(catch " ^ foo ^ ")"
  | Lcaml_alloc len -> "(ALLOC " ^ string_of_int len ^ ")"
  | Ldefine(name, args, body) -> "(define " ^ name ^ " " ^ String.concat " " (List.map (fun (x,_) -> x) args) ^ " " ^ to_string body ^ ")"
  | Lnothing -> "nothing"
  | Lunreachable -> "unreachable"

let header = ["; vim: set ft=llvm:";
              "declare double @fabs(double)";
              "declare void @llvm.stackrestore(i8*)";
              "declare i8* @llvm.stacksave()";
              "@ret_ptr = global i8 0";
              "declare void @caml_raise_exn(i64*, i64*, i64*) noreturn";
              "declare void @caml_call_gc(i64*, i64*)";
              "@caml_exception_pointer = external global i64*";
              "@caml_young_ptr = external global i64*";
              "@caml_young_limit = external global i64*";
              "@caml_bottom_of_stack = external global i64*";
              "@caml_last_return_address  = external global i64*";
              ""]

let emit_header () = List.iter emit_string header

let constants : string list ref = ref []

let functions : (string * string * string list) list ref = ref []

let add_const str =
  if List.exists (fun x -> String.compare x str == 0) !constants
  then ()
  else constants := str :: !constants

let add_function (ret, str, args) =
  if List.exists (fun (_, x,_) -> String.compare x str == 0) !functions
  then ()
  else functions := (typename ret, str, List.map (fun _ -> "i" ^ string_of_int size_int ^ "*") args) :: !functions

let emit_function_declarations () =
  List.iter (fun (ret_type, name, args) -> emit_string ("declare " ^ ret_type ^ " @" ^ name ^
                                     "(" ^ String.concat "," args ^ ") nounwind")) !functions

let emit_constant_declarations () =
  List.iter (fun name -> emit_string ("@" ^ name ^ " = external global " ^ typename int_type)) !constants


let data = Emit.data

(* vim: set foldenable : *)
