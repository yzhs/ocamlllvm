open Cmm
open Emit
open Emitaux
open Aux
open Reg
open Mach

let error s = error ("Llvm_selectgen: " ^ s)

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

let signed = function
  | Byte_signed | Sixteen_signed | Thirtytwo_signed -> true
  | Byte_unsigned | Sixteen_unsigned | Thirtytwo_unsigned -> false
  | Word -> true (* This is not important, the signedness is only necessary to
                  * decide whether to sign-extend or zero-extend the value *)
  | Single | Cmm.Double | Double_u -> failwith "floating point numbers are neither sign- nor zero-extended"

let translate_mem = function
  | Byte_signed | Byte_unsigned -> byte
  | Sixteen_signed | Sixteen_unsigned -> Integer 16
  | Thirtytwo_signed | Thirtytwo_unsigned -> Integer 32
  | Word -> addr_type
  | Single | Cmm.Double | Double_u -> Double (* TODO handle single precision floating point numbers *)

let get_type name =
  try Hashtbl.find types name
  with Not_found -> (*error ("Could not find identifier " ^ name ^ ".")*) addr_type

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


(* Functions used to make inserting certain common instructions easier *)
let comment seq str = ignore (insert seq (Icomment str) [||] Nothing Void)

let alloca seq name typ =
  assert (typ <> Void);
  add_type name (Address addr_type);
  insert seq Ialloca [||] (Reg(name, Address (if is_addr typ then addr_type else typ))) typ

let load seq arg reg typ = insert seq Iload [|arg|] (new_reg reg typ) typ
let store seq value addr typ = ignore (insert seq Istore [|value; addr|] Nothing typ)

let call seq fn args res typ =
  insert seq (Icall fn) args (new_reg res (ret_type typ)) typ
let extcall seq fn args res typ alloc =
  insert seq (Iextcall(fn, alloc)) args (new_reg res (ret_type typ)) typ

let ifthenelse seq cond ifso ifnot =
  let typ = typeof (last_instr ifso).res in
  let typ = if typ = Void then typeof (last_instr ifnot).res else typ in
  insert seq (Iifthenelse(ifso, ifnot)) [|cond|] (if typ = Void then Nothing else (new_reg "if_result" typ)) typ

let binop seq op left right typ = insert seq (Ibinop op) [|left; right|] (new_reg "" typ) typ
let comp seq op left right typ = insert seq (Icomp op) [|left; right|] (new_reg "" bit) typ

let getelemptr seq addr offset reg typ =
  insert seq Igetelementptr [|addr; offset|] reg typ
(* }}} *)


(* very simple type inference algorithm used to determine which type an
 * identifier has *)
(* {{{ *)
let rec caml_type expect = function
  (* HACK this is used to allow multiple let bindings of the exact same
   * identifier that the object system needs for implementing classes. *)
  | Cconst_int 0 -> addr_type
  | Cconst_int _ | Cconst_natint _ -> int_type
  | Cconst_float _ -> Double
  | Cconst_symbol _ | Cconst_pointer _ | Cconst_natpointer _ -> addr_type
  | Cvar id ->
      let name = translate_id id in
      if not (Hashtbl.mem types name) then
        add_type name (*if expect = Any then addr_type else expect*) addr_type;
      Hashtbl.find types name
  | Clet(id,arg,body) ->
      let name = translate_id id in
      (*let typ = caml_type Any arg in*)
      if not (Hashtbl.mem types name) || not (is_function (get_type name)) then
        add_type name (*if typ = Any || typ = Void then addr_type else typ*) addr_type;
      caml_type expect body
  | Cassign(id,expr) ->
      (*let typ = caml_type Any expr in*)
      add_type (translate_id id) (*if typ = Any || typ = Void then addr_type else typ*) addr_type;
      (*
      expect
       *)
      addr_type
  | Ctuple exprs ->
      ignore (List.map (caml_type Any) exprs);
      addr_type
  | Cop(Capply(typ, debug), exprs) ->
      ignore (List.map (caml_type Any) exprs);
      addr_type
  | Cop(Cextcall(_, _, _, _), exprs) ->
      ignore (List.map (caml_type Any) exprs);
      addr_type
  | Cop(Calloc, exprs) ->
      ignore (List.map (caml_type Any) exprs);
      addr_type
  | Cop(Cstore _, [addr; value]) ->
      ignore (caml_type Any value);
      ignore (caml_type addr_type addr);
      Void
  | Cop(Craise _, args) ->
      ignore (List.map (caml_type Any) args);
      Void
  | Cop(Ccheckbound _, [arr; index]) ->
      ignore (caml_type addr_type arr);
      ignore (caml_type int_type index);
      Void
  | Cop((Caddi|Csubi|Cmuli|Cdivi|Cmodi|Cand|Cor|Cxor|Clsl|Clsr|Casr), [left;right]) ->
      ignore (caml_type int_type left);
      ignore (caml_type int_type right);
      int_type
  | Cop((Caddf|Csubf|Cmulf|Cdivf), [left;right]) ->
      ignore (caml_type Double left);
      ignore (caml_type Double right);
      Double
  | Cop((Cadda|Csuba), [left;right]) ->
      ignore (caml_type addr_type left);
      ignore (caml_type addr_type right);
      addr_type
  | Cop(Ccmpf _, [left;right]) ->
      ignore (caml_type Double left);
      ignore (caml_type Double right);
      int_type
  | Cop(Ccmpa _, [left;right]) ->
      ignore (caml_type addr_type left);
      ignore (caml_type addr_type right);
      int_type
  | Cop(Ccmpi _, [left;right]) ->
      ignore (caml_type Any left);
      ignore (caml_type Any right);
      int_type
  | Cop(Cfloatofint, [arg]) ->
      ignore (caml_type int_type arg);
      Double
  | Cop(Cintoffloat, [arg]) ->
      ignore (caml_type Double arg);
      int_type
  | Cop((Cabsf|Cnegf), [arg]) ->
      ignore (caml_type Double arg);
      Double
  | Cop(Cload mem, [arg]) ->
      let typ = translate_mem mem in
      ignore (caml_type (Address typ) arg);
      if Double = typ then Double else addr_type
  | Cop(_,_) -> error "operation not available"
  | Csequence(fst,snd) ->
      ignore (caml_type Any fst);
      caml_type expect snd
  | Cifthenelse(cond, expr1, expr2) ->
      ignore (caml_type int_type cond);
      let typ = caml_type expect expr1 in
      let typ2 = caml_type (if typ <> Void then typ else expect) expr2 in
      if typ <> Void then typ else typ2
  | Cswitch(expr,is,exprs) ->
      let typ = ref Void in
      Array.iter (fun x -> let t = caml_type expect x in if t <> Void then typ := t) exprs;
      !typ
  | Cloop expr ->
      ignore (caml_type Void expr);
      Void
  | Ccatch(i,ids,expr1,expr2) ->
      ignore (caml_type expect expr1);
      ignore (caml_type expect expr2);
      expect
  | Cexit(i,exprs) ->
      ignore (List.map (caml_type expect) exprs);
      expect
  | Ctrywith(try_expr, id, with_expr) ->
      let typ = caml_type expect try_expr in
      add_type (translate_id id) addr_type; (* the exception's type *)
      let with_typ = caml_type expect with_expr in
      if typ = Void then with_typ else typ
(* }}} *)

let translate_float f =
  let x = Int64.bits_of_float (float_of_string f) in
  "0x" ^ Printf.sprintf "%Lx" x

let reverse_instrs tmp = let seq = ref dummy_instr in List.iter (insert_instr_debug seq) !tmp; !seq

let null = const "null" addr_type;;
let zero = const "0" int_type;;


let rec compile_instr seq = function
  | Cconst_int i ->
      insert seq (Ibinop Op_addi) [|const (string_of_int i) int_type; zero|] (new_reg "" int_type) int_type
  | Cconst_natint i ->
      insert seq (Ibinop Op_addi) [|const (Nativeint.to_string i) int_type; zero|] (new_reg "" int_type) int_type
  | Cconst_float f ->
      insert seq (Ibinop Op_addf) [|const (translate_float f) Double; const "0.0" Double|] (new_reg "" Double) Double
  | Cconst_symbol s ->
      let name = translate_symbol s in
      let typ = try Hashtbl.find types name with Not_found -> addr_type in
      if not (is_function typ) then Emit_common.add_const name;
      let typ = if is_addr typ then typ else addr_type in
      getelemptr seq (global name typ) zero (new_reg name addr_type) addr_type
  | Cconst_pointer i ->
      getelemptr seq (const (string_of_int i) int_type) zero (new_reg "" addr_type) addr_type
  | Cconst_natpointer i ->
      getelemptr seq (const (Nativeint.to_string i) int_type) zero (new_reg "" addr_type) addr_type

  | Cvar id ->
      print_debug "Cvar";
      let name = translate_id id in
      let typ = get_type name in
      load seq (Reg(name, typ)) "" (try deref typ with Cast_error s -> error ("derefencing type of " ^ name ^ " failed"))
  | Clet(id, value, body) ->
      print_debug "Clet";
      let name = translate_id id in
      let typ = get_type name in
      assert (typ <> Void);
      let value = compile_instr seq value in
      let var = alloca seq name typ in
      store seq value var typ;
      let result = compile_instr seq body in
       (* This store might be the last instruction in a block.  Therefore it
        * should return the actual result computed in that block: [res] *)
      insert seq Istore [|null; var|] result typ
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
      print_debug "Capply direct...";
      let arg_types = List.map (fun _ -> addr_type) args in
      let typ = function_type addr_type arg_types in
      Emit_common.add_function (addr_type, Emit_common.calling_conv, translate_symbol s, arg_types);
      add_type (translate_symbol s) typ;
      call seq (compile_instr seq symb) (store_and_load_args seq args) "call" typ
  | Cop(Capply(typ, debug), clos :: args) ->
      print_debug "Capply indirect...";
      ignore (insert seq (Icomment "applying function") [||] Nothing Void);
      let fn_type = function_type addr_type (List.map (fun _ -> addr_type) args) in
      let fn = compile_instr seq clos in
      call seq fn (store_and_load_args seq args) "call" fn_type
  | Cop(Capply(_,_), []) -> error "no function specified"
  | Cop(Cextcall(fn, typ, alloc, debug), args) ->
      print_debug "Cextcall";
      let arg_types = List.map (fun _ -> addr_type) args in
      let fn_type = function_type (translate_machtype typ) arg_types in
      Emit_common.add_function (translate_machtype typ, "ccc", fn, arg_types);
      add_type fn fn_type;
      extcall seq (global fn fn_type) (store_and_load_args seq args) "extcall" fn_type alloc

  | Cop(Calloc, args) -> (* TODO figure out how much space a single element needs *)
      print_debug "Calloc";
      let args = List.map (fun arg -> store_arg seq (compile_instr seq arg)) args in
      let ptr = insert seq (Ialloc (List.length args)) [||] (new_reg "alloc" addr_type) addr_type in
      let num = ref 0 in
      let emit_arg elem =
        let counter = string_of_int !num in
        num := !num + 1;
        let typ = typeof elem in
        let elemptr = getelemptr seq ptr (const counter int_type) (new_reg "" (typeof ptr)) addr_type in
        (*let value = if is_addr typ then load seq elem "" typ else elem in*)
        let value = load seq elem "" typ in
        store seq value elemptr typ
      in
      List.iter emit_arg args;
      getelemptr seq ptr (const "1" int_type) (new_reg "" (typeof ptr)) addr_type
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
      let cond = insert seq (Icomp Comp_le) [|arr; index|] (new_reg "" bit) (typeof index) in
      comment seq "checking bounds...";
      let ifso =
        let fn_type = function_type Void [] in
        instr_cons (Iextcall (global "caml_ml_array_bound_error" fn_type, false)) [||] Nothing fn_type
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
      let cond = insert seq (Icomp Comp_ne) [|const "0" int_type; cond|] (new_reg "" bit) int_type in
      ifthenelse seq cond (reverse_instrs ifso) (reverse_instrs ifnot)
  | Cswitch(expr, indices, exprs) ->
      print_debug "Cswitch";
      let value = compile_instr seq expr in
      let blocks = Array.map (fun x -> let seq = ref [] in ignore (compile_instr seq x); reverse_instrs seq) exprs in
      let typ = try typeof (List.find (fun x -> typeof (last_instr x).res <> Void) (Array.to_list blocks)).res with Not_found -> Void in
      Emit_common.add_const "caml_exn_Match_failure";
      insert seq (Iswitch(indices, blocks)) [|value|] (new_reg "switch" typ) typ
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
      insert seq (Icatch(i, body_instrs, handler_instrs)) [||] (if typ = Void then Nothing else new_reg "catch_foo" typ) typ
  | Cexit(i, exprs) ->
      print_debug "Cexit";
      List.iter (fun x -> ignore (compile_instr seq x)) exprs;
      insert seq (Iexit i) [||] Nothing Void
  | Ctrywith(try_expr, id, with_expr) ->
      print_debug "Ctrywith";
      let try_seq = ref [] in
      let with_seq = ref [] in
      ignore (compile_instr try_seq try_expr);
      ignore (getelemptr with_seq caml_exn (const "0" int_type) (Reg(translate_id id, addr_type)) addr_type);
      ignore (compile_instr with_seq with_expr);
      let try_instrs = reverse_instrs try_seq in
      let with_instrs = reverse_instrs with_seq in
      let typ = typeof (last_instr try_instrs).res in
      let typ = if typ = Void then typeof (last_instr with_instrs).res else typ in
      insert seq (Itrywith(try_instrs, with_instrs)) [||] (if typ = Void then Nothing else new_reg "try_with" typ) typ

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
      | Cadda -> getelemptr seq left right (new_reg "" (typeof left)) (Address byte)
      | Csuba ->
          let offset = binop seq Op_subi (const "0" int_type) right int_type in
          getelemptr seq left offset (new_reg "" (typeof left)) (Address byte)
      | _ -> error "Not a binary operator"
    end

  | [arg] -> begin
      print_debug "Cop unop";
      let arg = compile_instr seq arg in
      match op with
      | Cfloatofint -> insert seq Isitofp [|arg|] (new_reg "float_of_int" Double) Double
      | Cintoffloat -> insert seq Ifptosi [|arg|] (new_reg "int_of_float" float_sized_int) float_sized_int
      | Cabsf ->
          let fabs = global "fabs" (function_type Double [Double]) in
          extcall seq fabs [|arg|] "absf" (typeof fabs) false
      | Cnegf -> binop seq Op_subf (const "0.0" Double) arg Double
      | Cload mem ->
          let sign_or_zero_extend seq is_signed value typ =
            if is_int typ && typ <> int_type then
              insert seq (if is_signed then Isext else Izext) [|value|] (new_reg "" int_type) typ
            else value
          in
          let typ = translate_mem mem in
          let value = load seq arg "" typ in
          if Double = typ then value
          else sign_or_zero_extend seq (signed mem) value typ
      | _ -> error "Not a unary operator"
    end
  | _ -> error "There is no operator with this number of arguments"

and store_arg seq arg =
  let typ = typeof arg in
  if (*is_addr typ*) true then begin
    let name = reg_name (new_reg "arg" Any) in
    let name = String.sub name 1 (String.length name - 1) in
    let res = alloca seq name addr_type in
    comment seq "storing argument on the stack...";
    store seq arg res typ;
    res
  end else arg

and store_and_load_args seq args =
  let load_arg arg =
    let typ = typeof arg in
    if (*is_addr typ*) true then begin
      let res = load seq arg "" (deref typ) in
      store seq null arg (deref typ);
      res
    end else arg
  in
  let args = Array.map (fun arg -> store_arg seq (compile_instr seq arg)) (Array.of_list args) in
  Array.map load_arg args;;




let fundecl = function
  { fun_name = name; fun_args = args; fun_body = body; } ->
    instr_seq := dummy_instr;
    tmp_seq := [];
    reset_counter();
    Hashtbl.clear types;
    List.iter (fun (name, args) -> Hashtbl.add types (translate_symbol name) (function_type addr_type args)) !Emit_common.local_functions;
    Hashtbl.add types (translate_symbol name) (function_type addr_type (List.map (fun _ -> addr_type) args));
    ignore (caml_type Any body);
    let args = List.map (fun (x, typ) -> (translate_symbol (Ident.unique_name x), addr_type)) args in
    try
      let foo (x, typ) =
        (*
        let typ = try Hashtbl.find types x with Not_found -> addr_type in
        let typ = if is_int typ then typ else addr_type in
         *)
        store tmp_seq (Reg("param." ^ x, addr_type)) (alloca tmp_seq x addr_type) addr_type
      in
      List.iter foo args;
      let body = compile_instr tmp_seq body in

      ignore (insert tmp_seq Ireturn [|body|] Nothing (if typeof body <> Void then addr_type else Void));
      let argument_list = List.map (fun (id, _) -> "param." ^ id, addr_type) in
      List.iter (insert_instr_debug instr_seq) !tmp_seq;
      {name = translate_symbol name; args = argument_list args; body = !instr_seq}
    with (Llvm_error s as exn) ->
      print_endline ("error while compiling function " ^ name);
      print_endline (Printmach.instrs_to_string !instr_seq);
      raise exn


(* vim: set foldenable : *)
