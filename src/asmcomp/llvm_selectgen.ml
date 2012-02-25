open Cmm
open Emitaux
open Llvmemit
open Llvm_aux
open Llvm_mach

let error s = error ("Llvm_selectgen: " ^ s)

let label_counter = ref 0
let c () = label_counter := !label_counter + 1; string_of_int !label_counter

let types = Hashtbl.create 10

(* {{{ *)
let translate_op = function
  | Caddi -> Op_addi
  | Csubi -> Op_subi
  | Cmuli -> Op_muli
  | Cdivi -> Op_divi
  | Cmodi -> Op_modi
  | Cand  -> Op_and
  | Cor   -> Op_or
  | Cxor  -> Op_xor
  | Clsl  -> Op_lsl
  | Clsr  -> Op_lsr
  | Casr  -> Op_asr
  | Caddf -> Op_addf
  | Csubf -> Op_subf
  | Cmulf -> Op_mulf
  | Cdivf -> Op_divf
  | _ -> error "not a binary operator"

let translate_comp = function
    Ceq -> Comp_eq
  | Cne -> Comp_ne
  | Clt -> Comp_lt
  | Cle -> Comp_le
  | Cgt -> Comp_gt
  | Cge -> Comp_ge

let translate_mem = function
  | Byte_unsigned | Byte_signed -> Integer 8
  | Sixteen_unsigned | Sixteen_signed -> Integer 16
  | Thirtytwo_unsigned | Thirtytwo_signed -> Integer 32 
  | Word -> int_type
  | Single | Cmm.Double | Double_u -> Double (* TODO handle single precision floating point numbers *)

let translate_symbol s =
  let result = ref "" in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      |'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        result := !result ^ Printf.sprintf "%c" c
      | _ -> result := !result ^ Printf.sprintf "$%02x" (Char.code c)
  done;
  !result

let get_type name =
  try Hashtbl.find types name
  with Not_found -> error ("Could not find identifier " ^ name ^ ".")

let translate_id id = translate_symbol (Ident.unique_name id)

let translate_machtype typ =
  if typ = Cmm.typ_addr then addr_type
  else if typ = Cmm.typ_float then Double
  else if typ = Cmm.typ_int then int_type
  else if typ = Cmm.typ_void then addr_type (* HACK some extcalls of the same function are void calls while others return i64* *)
  else error "uknown machtype"
(* }}} *)

(* {{{ *)
let instr_seq = ref dummy_instr
let tmp_seq = ref []

let rec last_instr instr =
  match instr.next.desc with
    Iend -> instr
  | _ -> last_instr instr.next

let insert_instr_debug seq (desc, arg, res, typ, dbg) =
  seq := instr_cons_debug desc arg res typ dbg !seq

let insert seq desc arg res typ =
  seq := (desc, arg, res, typ, Debuginfo.none) :: !seq;
  res

let insert_debug seq desc dbg arg res typ =
  seq := (desc, arg, res, typ, dbg) :: !seq;
  res

let add_type name typ =
  Hashtbl.replace types name typ

let rec strip_addrs = function
  | Address (Address _ as typ) -> strip_addrs typ
  | typ -> typ

let comment seq str = ignore (insert seq (Icomment str) [||] Nothing Void)

let alloca seq name typ =
  assert (typ <> Void);
  add_type name (strip_addrs (Address typ));
  insert seq Ialloca [||] (Reg(name, strip_addrs (Address typ))) (deref (strip_addrs (Address typ)))

let load seq arg reg typ = insert seq Iload [|arg|] (register reg typ) typ
let store seq value addr typ = ignore (insert seq Istore [|value; addr|] Nothing typ)

let call seq fn args res typ =
  let ret = match typ with Address(Function(ret, _)) -> ret | _ -> error "not a function" in
  insert seq (Icall fn) args (register res ret) typ
let extcall seq fn args res typ =
  let ret = match typ with Address(Function(ret, _)) -> ret | _ -> error "not a function" in
  insert seq (Iextcall fn) args (register res ret) typ

let ifthenelse seq cond ifso ifnot =
  let typ = typeof (last_instr ifso).res in
  let typ = if typ = Void then typeof (last_instr ifnot).res else typ in
  insert seq (Iifthenelse(ifso, ifnot)) [|cond|] (if typ = Void then Nothing else (register "if_result" typ)) typ

let alloc seq len res typ = insert seq (Ialloc len) [||] (register res typ) typ

let binop seq op left right typ = insert seq (Ibinop op) [|left; right|] (register "" typ) typ
let comp seq op left right typ = insert seq (Icomp op) [|left; right|] (register "" bit) typ

let getelemptr seq addr offset typ =
  insert seq Igetelementptr [|addr; offset|] (register "" (typeof addr)) typ
(* }}} *)

let is_function = function
  | Function(_,_) | Address(Function(_,_)) -> true
  | _ -> false

(* very simple type inference algorithm used to determine which type an
 * identifier has *)
(* {{{ *)
let rec caml_type expect = function
  | Cconst_int _ -> addr_type
  | Cconst_natint _ -> int_type
  | Cconst_float _ -> Double
  | Cconst_symbol _ -> addr_type
  | Cconst_pointer _ -> addr_type
  | Cconst_natpointer _ -> addr_type
  | Cvar id ->
      let name = translate_id id in
      if not (Hashtbl.mem types name) then
        add_type name (if expect = Any then addr_type else expect);
      expect
  | Clet(id,arg,body) ->
      let name = translate_id id in
      let typ = caml_type Any arg in
      if not (Hashtbl.mem types name) || not (is_function (get_type name)) then
        add_type name (if typ = Any || typ = Void then addr_type else typ);
      caml_type expect body
  | Cassign(id,expr) ->
      let typ = caml_type Any expr in
      add_type (translate_id id) (if typ = Any || typ = Void then addr_type else typ);
      expect
  | Ctuple exprs -> ignore (List.map (caml_type Any) exprs); Any
  | Cop(Capply(typ, debug), exprs) -> ignore (List.map (caml_type Any) exprs); expect
  | Cop(Cextcall(fn, typ, alloc, debug), exprs) -> ignore (List.map (caml_type Any) exprs); expect
  | Cop(Calloc, exprs) -> List.iter (fun x -> ignore (caml_type Any x)) exprs; addr_type (* this is always the correct result type of an allocation *)
  | Cop(Cstore mem, [addr; value]) -> let typ = caml_type Any value in ignore (caml_type (Address typ) addr); Void
  | Cop(Craise debug, args) -> ignore (List.map (caml_type Any) args); Void
  | Cop(Ccheckbound debug, [arr; index]) -> ignore (caml_type int_type index); ignore (caml_type addr_type arr); Void
  | Cop(Ccheckbound _, _) -> error "not implemented: checkound with #args != 2"
  | Cop((Caddi|Csubi|Cmuli|Cdivi|Cmodi|Cand|Cor|Cxor|Clsl|Clsr|Casr), [left;right]) ->
      ignore (caml_type int_type left);
      ignore (caml_type int_type right);
      int_type
  | Cop((Caddf|Csubf|Cmulf|Cdivf), [left;right]) ->
      ignore (caml_type Double left); ignore (caml_type Double right); Double
  | Cop((Cadda|Csuba), [left;right]) ->
      ignore (caml_type addr_type left);
      ignore (caml_type addr_type right);
      addr_type
  | Cop(Ccmpi op, [left;right]) ->
      ignore (caml_type int_type left);
      ignore (caml_type int_type right);
      int_type
  | Cop(Ccmpf op, [left;right]) ->
      ignore (caml_type Double left);
      ignore (caml_type Double right);
      int_type
  | Cop(Ccmpa op, [left;right]) ->
      ignore (caml_type addr_type left); ignore (caml_type addr_type right); int_type
  | Cop(Cfloatofint, [arg]) -> ignore (caml_type int_type arg); Double
  | Cop(Cintoffloat, [arg]) -> ignore (caml_type Double arg); int_type
  | Cop(Cabsf, [arg]) -> ignore (caml_type Double arg); Double
  | Cop(Cnegf, [arg]) -> ignore (caml_type Double arg); Double
  | Cop(Cload mem, [arg]) ->
      let typ = translate_mem mem in
      ignore (caml_type (Address typ) arg);
      if not (is_float typ) then int_type else typ
  | Cop(_,_) -> error "operation not available"
  | Csequence(fst,snd) -> ignore (caml_type Any fst); caml_type expect snd
  | Cifthenelse(cond, expr1, expr2) ->
      ignore (caml_type int_type cond);
      let typ = caml_type expect expr1 in
      let typ2 = caml_type (if typ <> Void then typ else expect) expr2 in
      if typ <> Void then typ else typ2
  | Cswitch(expr,is,exprs) ->
      let typ = ref Void in
      Array.iter (fun x -> let t = caml_type expect x in if t <> Void then typ := t) exprs;
      !typ
  | Cloop expr -> ignore (caml_type Void expr); Void
  | Ccatch(i,ids,expr1,expr2) ->
      ignore (caml_type expect expr1);
      ignore (caml_type expect expr2);
      expect
  | Cexit(i,exprs) -> List.iter (fun x -> ignore (caml_type expect x)) exprs; expect
  | Ctrywith(try_expr, id, with_expr) ->
      let typ = caml_type expect try_expr in
      add_type (translate_id id) addr_type; (* the exception's type *)
      let with_typ = caml_type expect with_expr in
      if typ = Void then with_typ else typ
(* }}} *)

let fabs = Const("@fabs", Address(Function(Double, [Double])))

let reverse_instrs tmp = let seq = ref dummy_instr in List.iter (insert_instr_debug seq) !tmp; !seq

let rec compile_instr seq instr =
  match instr with
  | Cconst_int i ->
      insert seq (Ibinop Op_addi) [|Const(string_of_int i, int_type); Const("0", int_type)|] (register "" int_type) int_type
  | Cconst_natint i ->
      insert seq (Ibinop Op_addi) [|Const(Nativeint.to_string i, int_type); Const("0", int_type)|] (register "" int_type) int_type
  | Cconst_float f ->
      insert seq (Ibinop Op_addf) [|Const(f, Double); Const("0.0", Double)|] (register "" Double) Double
  | Cconst_symbol s ->
      let typ =
        try Hashtbl.find types (translate_symbol s)
        with Not_found -> addr_type
      in
      begin match typ with
      | Address(Function(_,_)) -> ()
      | Function(_,_) -> ()
      | _ -> add_const (translate_symbol s) (* TODO why not store the actual type of the symbol? *)
      end;
      insert seq Igetelementptr [|Const("@" ^ translate_symbol s, if is_addr typ then typ else Address typ); Const("0", int_type)|]
        (register (translate_symbol s) int_type) addr_type
  | Cconst_pointer i ->
      insert seq Igetelementptr [|Const(string_of_int i, int_type); Const("0", int_type)|] (register "" addr_type) addr_type
  | Cconst_natpointer i ->
      insert seq Igetelementptr [|Const(Nativeint.to_string i, int_type); Const("0", int_type)|] (register "" addr_type) addr_type
  | Cvar id ->
      print_debug "Cvar";
      let name = translate_id id in
      let typ = try deref (get_type name) with Cast_error s -> error ("dereferencing type of " ^ name ^ " failed") in
      load seq (Reg(name, Address typ)) "" typ
  | Clet(id, arg, body) ->
      print_debug "Clet";
      let name = translate_id id in
      let typ = get_type name in
      let res_arg = compile_instr seq arg in
      let addr = assert (typ <> Void); alloca seq name typ in (* TODO check whether the variable already exists *)
      store seq res_arg addr typ;
      compile_instr seq body
  | Cassign(id, expr) ->
      print_debug "Cassign";
      let name = translate_id id in
      let value = compile_instr seq expr in
      let typ = get_type name in
      store seq value (Reg(name, typ)) (deref typ);
      Nothing
  | Ctuple [] -> Nothing
  | Ctuple exprs ->
      (* TODO What is Ctuple used for? Implement that. *)
      Const("tuple_res", Void)
  | Cop(Capply(typ, debug), (Cconst_symbol s as symb) :: args) ->
      print_debug "Capply Ccons_symbol...";
      let args = List.map (compile_instr seq) args in
      let arg_types = Array.to_list (Array.make (List.length args) addr_type) in
      let typ = Address(Function(addr_type, arg_types)) in
      add_function (addr_type, calling_conv, translate_symbol s, arg_types);
      add_type (translate_symbol s) typ;
      call seq (compile_instr seq symb) (Array.of_list args) "call" typ
  | Cop(Capply(typ, debug), clos :: args) ->
      print_debug "Capply closure...";
      let args = List.map (compile_instr seq) args in
      let fn_type = Address(Function(addr_type, List.map (fun _ -> addr_type) args)) in
      let fn = compile_instr seq clos in
      call seq fn (Array.of_list args) "call" fn_type
  | Cop(Capply(_,_), []) -> error "no function specified"
  | Cop(Cextcall(fn, typ, alloc, debug), exprs) ->
      print_debug "Cextcall";
      let args = List.map (compile_instr seq) exprs in
      add_function (translate_machtype typ, "ccc", fn, args);
      let fn_type = Address(Function(translate_machtype typ, List.map (fun _ -> addr_type) args)) in
      add_type fn fn_type;
      extcall seq (Const("@" ^ fn, fn_type)) (Array.of_list args) "extcall" fn_type
  | Cop(Calloc, args) -> (* TODO figure out how much space a single element needs *)
      print_debug "Calloc";
      let args = List.map (compile_instr seq) args in
      let alloc = alloc seq (List.length args) "alloc" addr_type in
      let alloc = getelemptr seq alloc (Const("1", int_type)) addr_type in
      let num = ref (-1) in
      let emit_arg elem =
        let counter = string_of_int !num in
        let elemptr = getelemptr seq alloc (Const(counter, int_type)) addr_type in
        num := !num + 1;
        (* The store itself returns [Nothing] but the enclosing blocks result is
        * [alloc].                                  vvvvv                     *)
        ignore (insert seq Istore [|elem; elemptr|] alloc (typeof elem))
      in
      List.iter emit_arg args;
      alloc
  | Cop(Cstore mem, [addr; value]) ->
      print_debug "Cstore";
      store seq (compile_instr seq value) (compile_instr seq addr) (translate_mem mem);
      Nothing
  | Cop(Craise debug, [arg]) ->
      print_debug "Craise";
      insert_debug seq Iraise debug [|compile_instr seq arg|] Nothing Void
  | Cop(Craise _, _) -> error "wrong number of arguments for Craise"
  | Cop(Ccheckbound debug, [arr; index]) ->
      print_debug "Ccheckbound";
      let arr = compile_instr seq arr in
      let index = compile_instr seq index in
      assert (typeof arr <> Void);
      let cond = insert seq (Icomp Comp_le) [|arr; index|] (register "" bit) (typeof index) in
      comment seq "checking bounds...";
      let ifso =
        instr_cons (Iextcall (Const("@caml_ml_array_bound_error", Address(Function(Void, []))))) [||] Nothing (Address (Function(Void, [])))
          (instr_cons Iunreachable [||] Nothing Void dummy_instr)
      in
      ifthenelse seq cond ifso dummy_instr
  | Cop(Ccheckbound _, _) -> error "not implemented: checkound with #args != 2"
  | Cop(op, exprs) -> compile_operation seq op exprs
  | Csequence(fst,snd) ->
      print_debug "Csequence";
      ignore (compile_instr seq fst); compile_instr seq snd
  | Cifthenelse(cond, expr1, expr2) ->
      print_debug "Cifthenelse";
      let cond = compile_instr seq cond in
      let ifso = ref [] in
      let ifnot = ref [] in
      ignore (compile_instr ifso expr1);
      ignore (compile_instr ifnot expr2);
      let cond = insert seq (Icomp Comp_ne) [|Const("0", int_type); cond|] (register "" bit) int_type in
      ifthenelse seq cond (reverse_instrs ifso) (reverse_instrs ifnot)
  | Cswitch(expr, indices, exprs) ->
      print_debug "Cswitch";
      let value = compile_instr seq expr in
      let blocks = Array.map (fun x -> let seq = ref [] in ignore (compile_instr seq x); reverse_instrs seq) exprs in
      let typ = try typeof (List.find (fun x -> typeof (last_instr x).res <> Void) (Array.to_list blocks)).res with Not_found -> Void in
      add_const "caml_exn_Match_failure";
      insert seq (Iswitch(indices, blocks)) [|value|] (register "switch" typ) typ
  | Cloop expr ->
      print_debug "Cloop";
      let lseq = ref [] in
      ignore (compile_instr lseq expr);
      insert seq (Iloop (reverse_instrs lseq)) [||] Nothing Void
  | Ccatch(i, ids, body, handler) ->
      print_debug "Ccatch";
      let fn id =
        let id = translate_id id in
        add_type id addr_type;
        ignore (alloca seq id addr_type)
      in
      List.iter fn ids;
      let instr_body = ref [] in
      let instr_handler = ref [] in
      ignore (compile_instr instr_body body);
      ignore (compile_instr instr_handler handler);
      let body_instrs = reverse_instrs instr_body in
      let handler_instrs = reverse_instrs instr_handler in
      let typ = typeof (last_instr body_instrs).res in
      let typ = if typ = Void then typeof (last_instr handler_instrs).res else typ in
      insert seq (Icatch(i, body_instrs, handler_instrs)) [||] (if typ = Void then Nothing else register "catch_foo" typ) typ
  | Cexit(i, exprs) ->
      print_debug "Cexit";
      List.iter (fun x -> ignore (compile_instr seq x)) exprs;
      insert seq (Iexit i) [||] Nothing Void
  | Ctrywith(try_expr, id, with_expr) ->
      print_debug "Ctrywith";
      let try_seq = ref [] in
      let with_seq = ref [] in
      ignore (compile_instr try_seq try_expr);
      ignore (insert with_seq Igetelementptr [|Const("@caml_exn", Address addr_type); Const("0", int_type)|] (Reg(translate_id id, addr_type)) addr_type);
      ignore (compile_instr with_seq with_expr);
      let try_instrs = reverse_instrs try_seq in
      let with_instrs = reverse_instrs with_seq in
      let typ = typeof (last_instr try_instrs).res in
      let typ = if typ = Void then typeof (last_instr with_instrs).res else typ in
      insert seq (Itrywith(try_instrs, with_instrs)) [||] (if typ = Void then Nothing else register "try_with" typ) typ

and compile_operation seq op = function
  | [l;r] -> begin
      print_debug "Cop binop";
      let left = compile_instr seq l in
      let right = compile_instr seq r in
      match op with
      | Caddi | Csubi | Cmuli | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl | Clsr
      | Casr -> binop seq (translate_op op) left right int_type
      | Caddf | Csubf | Cmulf | Cdivf ->
          binop seq (translate_op op) left right Double
      | Ccmpi op -> comp seq (translate_comp op) left right int_type
      | Ccmpf op -> comp seq (translate_comp op) left right Double
      | Ccmpa op -> comp seq (translate_comp op) left right addr_type
      | Cadda -> getelemptr seq left right (Address byte)
      | Csuba ->
          let offset = binop seq Op_subi (Const("0", int_type)) right int_type in
          getelemptr seq left offset (Address byte)
      | _ -> error "Not a binary operator"
    end

  | [arg] -> begin
      print_debug "Cop unop";
      let arg = compile_instr seq arg in
      match op with
      | Cfloatofint -> insert seq Isitofp [|arg|] (register "float_of_int" Double) Double
      | Cintoffloat -> insert seq Ifptosi [|arg|] (register "int_of_float" float_sized_int) float_sized_int
      | Cabsf -> extcall seq fabs [|arg|] "absf_res" (Address(Function(Double, [Double])))
      | Cnegf -> binop seq Op_subf (Const("0.0", Double)) arg Double
      | Cload mem ->
          let typ = translate_mem mem in
          load seq arg "" typ
      | _ -> error "Not a unary operator"
    end
  | _ -> error "There is no operator with this number of arguments"


let fundecl = function
  { fun_name = name; fun_args = args; fun_body = body; } ->
    instr_seq := dummy_instr;
    tmp_seq := [];
    label_counter := 0;
    reset_counter();
    Hashtbl.clear types;
    List.iter (fun (name, args) -> Hashtbl.add types (translate_symbol name) (Address(Function(addr_type, args)))) !local_functions;
    Hashtbl.add types (translate_symbol name) (Address(Function(addr_type, List.map (fun _ -> addr_type) args)));
    ignore (caml_type Any body);
    let args = List.map (fun (x, typ) -> (translate_symbol (Ident.unique_name x), addr_type)) args in
    try
      let foo (x, typ) =
        let typ = try Hashtbl.find types x with Not_found -> addr_type in
        let typ = if is_int typ then typ else addr_type in
        store tmp_seq (Reg("param." ^ x, addr_type)) (assert (typ <> Void); alloca tmp_seq x typ) typ
      in
      List.iter foo args;
      let body = compile_instr tmp_seq body in

      ignore (insert tmp_seq Ireturn [|body|] Nothing (if typeof body <> Void then addr_type else Void));
      let argument_list = List.map (fun (id, _) -> "param." ^ id, addr_type) in
      List.iter (insert_instr_debug instr_seq) !tmp_seq;
      {name = translate_symbol name; args = argument_list args; body = !instr_seq}
    with Llvm_error s ->
      print_endline ("error while compiling function " ^ name);
      print_endline (to_string !instr_seq);
      error s


(* vim: set foldenable : *)
