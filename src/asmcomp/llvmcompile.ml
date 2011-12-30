open Cmm
open Emitaux
open Llvmemit
open Llvm_types

let label_counter = ref 0
let c () = label_counter := !label_counter + 1; string_of_int !label_counter

let types = Hashtbl.create 10

let is_addr = function Address _ -> true | _ -> false
let is_float = function Double -> true | _ -> false
let is_int = function Integer _ -> true | _ -> false

let vars = Hashtbl.create 10
let is_def = Hashtbl.mem vars

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

let translate_mem = function
  | Byte_unsigned | Byte_signed -> Integer 8
  | Sixteen_unsigned | Sixteen_signed -> Integer 16
  | Thirtytwo_unsigned | Thirtytwo_signed -> Integer 32 
  | Word -> int_type
  | Single | Cmm.Double | Double_u -> Double

let translate_fcomp = function
  | Ceq -> "fcmp oeq"
  | Cne -> "fcmp one"
  | Clt -> "fcmp olt"
  | Cle -> "fcmp ole"
  | Cgt -> "fcmp ogt"
  | Cge -> "fcmp oge"

let translate_icomp = function
  | Ceq -> "icmp eq"
  | Cne -> "icmp ne"
  | Clt -> "icmp slt"
  | Cle -> "icmp sle"
  | Cgt -> "icmp sgt"
  | Cge -> "icmp sge"

let translate_ucomp = function
  | Ceq -> "icmp eq"
  | Cne -> "icmp ne"
  | Clt -> "icmp ult"
  | Cle -> "icmp ule"
  | Cgt -> "icmp ugt"
  | Cge -> "icmp uge"

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

let translate_machtype typ =
  if typ = Cmm.typ_addr then addr_type
  else if typ = Cmm.typ_float then Double
  else if typ = Cmm.typ_int then int_type
  else if typ = Cmm.typ_void then addr_type (* HACK some extcalls of the same function are void calls while others return i64* *)
  else error "uknown machtype"
(* }}} *)

(* {{{ *)
let cast value dest_typ =
  let typ = typeof value in
  match typ, dest_typ with
    | (Integer i, Integer j) ->
      if i == j
      then value
      else if i < j
      then Lcast(Zext, value, typ, dest_typ)
      else Lcast(Trunc, value, typ, dest_typ)
    | (Integer i, Address _) ->
      if i == size_int * 8
      then Lcast(Inttoptr, value, typ, dest_typ)
      else error ("could not cast integer of size " ^ string_of_int i ^ " to pointer")
    | (Integer i, Double) ->
      if i == size_float * 8
      then Lcast(Bitcast, value, typ, dest_typ)
      else error ("could not cast integer of size " ^ string_of_int i ^ " to pointer")
    | (Integer _, Function(_,_)) -> Lcast(Inttoptr, value, typ, dest_typ)
    | (Integer _, _) -> error ("invalid cast from integer to " ^ string_of_type dest_typ)
    | (Double, Integer i) ->
      if i == size_float * 8
      then Lcast(Bitcast, value, typ, dest_typ)
      else error ("could not cast float to integer of size " ^ string_of_int i)
    | (Address i, Address j) -> if i == j then value else Lcast(Bitcast, value, typ, dest_typ)
    | (Address _, Integer i) -> Lcast(Ptrtoint, value, typ, dest_typ)
    | (Double, Address _) ->
        Lcast(Inttoptr, Lcast(Bitcast, value, typ, float_sized_int), float_sized_int, dest_typ)
    | (a, b) ->
      if a == b
      then value
      else error ("error while trying to cast " ^ string_of_type typ ^
                     " to " ^ string_of_type dest_typ ^ ": " ^
                     (match emit_llvm value with Just s -> s | Error s -> s))

let alloca name typ =
  Hashtbl.add vars name ();
  Hashtbl.add types name (Address typ);
  Lalloca(name, typ)

let load addr = Lload addr

let local_load addr typ = Lload(Lvar(addr, typ))

let store value addr =
  assert (typeof value <> Void);
  assert (is_addr (typeof addr));
  Lstore(cast value (deref (typeof addr)), addr)

let store_non_void arg addr =
  if (try typeof arg with Llvm_error _ -> Void) == Void
  then arg
  else store arg addr

let binop op typ left right = Lbinop(op, typ, cast left typ, cast right typ)

let comp op typ left right = Lcomp(op, typ, cast left typ, cast right typ)

let getelementptr addr offset =
  cast (Lgetelementptr(cast addr (Address (Integer 8)), offset)) addr_type

let load_exn_ptr () = local_load "@caml_exeption_pointer" (Address addr_type)
let load_young_ptr () = local_load "@caml_young_ptr" (Address addr_type)

let call typ fn args =
  let args = List.map (fun a -> assert (typeof a <> Void); cast a addr_type) args in
  Lcall(addr_type, fn, args)

let ccall typ fn args =
  let args = List.map (fun a -> assert (typeof a <> Void); cast a addr_type) args in
  Lccall(typ, fn, args)

let return value = Lreturn(value, typeof value)
(* }}} *)

(* {{{ *)
let rec caml_type expect = function
  | Cconst_int _ -> int_type
  | Cconst_natint _ -> int_type
  | Cconst_float _ -> Double
  | Cconst_symbol _ -> addr_type
  | Cconst_pointer _ -> addr_type
  | Cconst_natpointer _ -> addr_type
  | Cvar id ->
      let name = Ident.unique_name id in
      if expect != Any && not (Hashtbl.mem types name) then
        Hashtbl.add types (translate_symbol (Ident.unique_name id)) expect;
      expect
  | Clet(id,arg,body) ->
      let name = translate_symbol (Ident.unique_name id) in
      let typ = caml_type Any arg in
      Hashtbl.add types name (Address typ);
      caml_type expect body
  | Cassign(id,expr) -> Void
  | Ctuple [] -> Void
  | Ctuple exprs -> Void (* this is probably wrong... *)
  | Cop(Capply(typ, debug), exprs) -> expect
  | Cop(Cextcall(fn, typ, alloc, debug), exprs) -> expect
  | Cop(Calloc, _) -> addr_type (* this is always the correct result type of an allocation *)
  | Cop(Cstore mem, [addr; value]) -> Void
  | Cop(Craise debug, args) -> Void
  | Cop(Ccheckbound debug, [arr; index]) -> Void
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
      let typ = caml_type Any expr1 in caml_type typ expr2
  | Cswitch(expr,is,exprs) -> expect (* TODO figure out the real type *)
  | Cloop expr -> ignore (caml_type Any expr); Void
  | Ccatch(i,ids,expr1,expr2) ->
      ignore (caml_type expect expr1);
      ignore (caml_type expect expr2);
      expect
  | Cexit(i,exprs) -> Void (* TODO process exprs *)
  | Ctrywith(try_expr, id, with_expr) ->
      ignore (caml_type expect try_expr);
      Hashtbl.add types (translate_symbol (Ident.unique_name id)) addr_type; (* the exception's type *)
      caml_type expect with_expr
(* }}} *)

let exits = Hashtbl.create 10

let rec helper in_tail_position in_try_block instr =
  match instr with
  | Cconst_int i -> Lconst(string_of_int i, int_type)
  | Cconst_natint i -> Lconst(Nativeint.to_string i, int_type)
  | Cconst_float f -> Lconst(f, Double)
  | Cconst_symbol s ->
      let typ = try Address (Hashtbl.find types s) with Not_found -> addr_type in
      begin match typ with
      | Address (Function(_,_)) -> ()
      | _ -> add_const s
      end;
      Lconst("@" ^ translate_symbol s, typ)
  | Cconst_pointer i ->
      cast (Lconst(string_of_int i, int_type)) addr_type
  | Cconst_natpointer i ->
      cast (Lconst(Nativeint.to_string i, int_type)) addr_type

  | Cvar id ->
      let name = translate_symbol (Ident.unique_name id) in
      let typ =
        try Hashtbl.find types name
        with Not_found -> error ("Could not find identifier " ^ name ^ ".")
      in
      local_load ("%" ^ name) typ
  | Clet(id,arg,body) ->
      let name = translate_symbol (Ident.unique_name id) in
      let res_arg = helper false in_try_block arg in
      let type_arg = typeof res_arg in
      let typ = if Double == type_arg then Double else int_type in
      let res = if is_def name then res_arg else alloca name typ in
      store res_arg res @@ helper in_try_block in_tail_position body
  | Cassign(id,expr) ->
      let res = helper false in_try_block expr in
      let name = translate_symbol (Ident.unique_name id) in
      let mem_typ =
        try Hashtbl.find types name
        with Not_found -> error ("not found: " ^ name)
      in
      store res (Lvar("%" ^ name, mem_typ))
  | Ctuple [] -> Lconst(";", Void)
  | Ctuple exprs -> begin
      (* TODO What is Ctuple used for? Implement that. *)
      Lconst("tuple_res", Void)
    end

  | Cop(Capply(typ, debug), Cconst_symbol s :: args) ->
      let args = compile_list in_try_block args in
      add_function (addr_type, calling_conv, translate_symbol s, args);
      call addr_type (Lvar("@" ^ translate_symbol s, addr_type)) args
  | Cop(Capply(typ, debug), clos :: args) ->
      let args = compile_list in_try_block args in
      let fn_type = Address(Function(addr_type, List.map (fun x -> addr_type) args)) in
      let tmp = helper false in_try_block clos in
      assert (typeof tmp <> Void);
      let fn = cast tmp fn_type in
      call addr_type fn args
  | Cop(Capply(_,_), []) -> error "no function specified"
  | Cop(Cextcall(fn, typ, alloc, debug), exprs) ->
      let args = compile_list in_try_block exprs in
      add_function (translate_machtype typ, "ccc", fn, args);
      ccall (translate_machtype typ) (Lvar("@" ^ fn, addr_type)) args

  | Cop(Calloc, args) ->
      let c = c() in
      let data = Lcaml_alloc (List.length args) in (* TODO figure out how much space a single element needs *)
      let args = List.map (helper false in_try_block) args in
      let num = ref (-size_int) in
      let ptr = load (Lvar("%alloc" ^ c, Address addr_type)) in
      let emit_arg x =
        let counter = string_of_int !num in
        let header = getelementptr ptr (Lconst(string_of_int size_int, int_type)) in
        let elemptr = getelementptr header (Lconst(counter, int_type)) in
        num := !num + size_int;
        store x elemptr
      in
      store data (alloca ("alloc" ^ c) addr_type)
      @@ List.fold_left (fun a b -> a @@ emit_arg b) ptr args
      @@ getelementptr ptr (Lconst(string_of_int size_int, int_type))
  | Cop(Cstore mem, [addr; value]) ->
      let addr = helper false in_try_block addr in
      let value = helper false in_try_block value in
      if typeof value == Address (translate_mem mem)
      then begin
        assert (typeof addr <> Void);
        store value (cast addr (typeof value))
      end else begin
        assert (typeof value <> Void);
        store (cast value (translate_mem mem)) (cast addr (Address (translate_mem mem)))
      end
  | Cop(Craise debug, [arg]) ->
      let tmp = helper false in_try_block arg in
      assert (typeof tmp <> Void);
      Lcaml_raise_exn (cast tmp addr_type)
  | Cop(Craise _, _) -> error "wrong number of arguments for Craise"
  | Cop(Ccheckbound debug, [arr; index]) ->
      let arr = helper false in_try_block arr in
      let index = helper false in_try_block index in
      assert (typeof arr <> Void);
      let header = getelementptr (cast arr addr_type) (Lconst("-" ^ string_of_int size_addr, int_type)) in
      let length = load header in
      let cond = comp "icmp ule" (typeof length) index length in
      let c = c () in
      add_function (addr_type, "ccc", "caml_ml_array_bound_error", []);
      Lcomment "checking bounds..."
      @@ Lbr_cond(cond, "out_of_bounds" ^ c, "ok" ^ c)
      @@ Llabel ("out_of_bounds" ^ c)
      @@ call Void (Lvar("@caml_ml_array_bound_error",Any)) []
      @@ Lbr ("ok" ^ c)
      @@ Llabel ("ok" ^ c)
  | Cop(Ccheckbound _, _) -> error "not implemented: checkound with #args != 2"
  | Cop(op, exprs) -> compile_operation in_try_block op exprs

  | Csequence(fst,snd) -> helper false in_try_block fst @@ helper in_tail_position in_try_block snd
  | Cifthenelse(cond, expr1, expr2) -> begin
      let in_tail_position = false in
      let c = c () in
      let cond = helper false in_try_block cond in
      let then_res = helper in_tail_position in_try_block expr1 in
      let else_res = helper in_tail_position in_try_block expr2 in
      let typ = if typeof then_res != Void then typeof then_res else typeof else_res in
      let cond = comp "icmp ne" int_type (Lconst("0", int_type)) (if typeof cond == int_type then cond else load cond) in
      let if_res = Lvar("%if_res" ^ c, Address (if typ != Void then typ else int_type)) in
      (* TODO emit different code when in tail call position *)
      let res =
        alloca ("if_res" ^ c) (if typ != Void then typ else int_type) (* if typ == Void, if_res is never used *)
        @@ Lbr_cond(cond, "then" ^ c, "else" ^ c) @@ Llabel ("then" ^ c)
        @@ (if in_tail_position then return then_res else store_non_void then_res if_res)
        @@ Lbr ("fi" ^ c) @@ Llabel ("else" ^ c)
        @@ (if in_tail_position then return then_res else store_non_void else_res if_res)
        @@ Lbr ("fi" ^ c)
        @@ Llabel ("fi" ^ c)
      in if typ != Void then res @@ local_load ("%if_res" ^ c) (Address typ) else res
    end
  | Cswitch(expr,indexes,exprs) ->
      let exprs = Array.map (fun x -> ignore (c()); helper in_tail_position in_try_block x) exprs in
      let c = c () in
      let typ = try typeof (List.find (fun x -> typeof x != Void) (Array.to_list exprs)) with Not_found -> Void in
      let value = alloca ("switch_res" ^ c) (if typ != Void then typ else int_type) @@ helper false in_try_block expr in
      let create_block lbl expr = Llabel (lbl ^ "." ^ c) @@ expr @@ Lbr ("end." ^ c) in
      add_const "caml_exn_Match_failure";
      let default = create_block "default" (Lcaml_raise_exn(Lvar("@caml_exn_Match_failure", addr_type))) in
      let blocks = Array.mapi (fun i expr -> create_block ("case" ^ string_of_int i) expr) exprs in
      Lswitch(c, value, indexes, default, blocks, typ)
      @@ Llabel ("end." ^ c)
      @@ (if typ == Void then Lnothing else Lload(Lvar("%switch_res" ^ c, Address typ)))
  | Cloop expr ->
      let c = c () in
      Lbr ("loop" ^ c)
      @@ Llabel ("loop" ^ c)
      @@ helper false in_try_block expr
      @@ Lbr ("loop" ^ c)
  | Ccatch(i,ids,expr1,expr2) ->
      let c = c () in
      let ids =
        let fn id =
          let id = translate_symbol (Ident.unique_name id) in
          Hashtbl.add types id addr_type;
          alloca id addr_type
        in
        List.map fn ids
      in
      Hashtbl.add exits i c;
      let expr1 = helper false true expr1
      and expr2 = helper false in_try_block expr2 in
      Hashtbl.remove exits i;
      Lcomment "catching..."
      @@ List.fold_left (@@) Lnothing ids
      @@ expr1
      @@ Lbr ("exit" ^ string_of_int i ^ "." ^ c)
      @@ Llabel ("exit" ^ string_of_int i ^ "." ^ c)
      @@ expr2
      @@ Lcomment "done catching"
  | Cexit(i,exprs) ->
      let exprs = List.map (fun x -> (helper false in_try_block x)) exprs in
      Lcomment "exiting loop"
      @@ List.fold_left (@@) Lnothing exprs
      @@ Lbr ("exit" ^ string_of_int i ^ "." ^ Hashtbl.find exits i)
  | Ctrywith(try_expr, id, with_expr) ->
      Hashtbl.add types (translate_symbol (Ident.unique_name id)) addr_type;
      let counter = c () in
      let res = helper in_tail_position in_try_block try_expr in
      let with_res = helper in_tail_position in_try_block with_expr in
      let typ = if typeof res != Void then typeof res else typeof with_res in
      let try_with_res = Lvar("%try_with_res" ^ counter, if typ == Void then int_type else Address typ) in
      let trywith =
        Lcomment "begin of try-with-block"
        @@ alloca ("try_with_res" ^ counter) (if typ != Void then typ else int_type)
        @@ store_non_void res try_with_res
        @@ Lcomment "end of try block"
        @@ Lcaml_catch_exn(translate_symbol (Ident.unique_name id), with_res, try_with_res)
        @@ Lcomment "end of with block"
      in
      if typ == Void then trywith else trywith @@ local_load ("%try_with_res" ^ counter) (Address typ)

and compile_operation in_try_block op = function
  | [l;r] -> begin
      let left = helper false in_try_block l in
      let right = helper false in_try_block r in
      match op with
      | Caddi | Csubi | Cmuli | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl | Clsr | Casr ->
          binop (translate_op op) int_type left right
      | Caddf | Csubf | Cmulf | Cdivf ->
          binop (translate_op op) Double left right
      | Ccmpi op ->
          cast (comp (translate_icomp op) int_type left right) int_type
      | Ccmpf op ->
          cast (comp (translate_fcomp op) Double left right) int_type
      | Ccmpa op ->
          cast (comp (translate_ucomp op) int_type left right) int_type
      | Cadda -> getelementptr (cast left addr_type) (cast right int_type)
      | Csuba -> getelementptr (cast left addr_type) (binop Op_subi int_type (Lconst("0", int_type)) (cast right int_type))
      | _ -> error "Not a binary operator"
      end

  | [arg] -> begin
      let arg = helper false in_try_block arg in
      match op with
      | Cfloatofint -> Lcast(Sitofp, arg, float_sized_int, Double)
      | Cintoffloat -> Lcast(Fptosi, arg, Double, float_sized_int)
      | Cabsf -> Lccall(Double, Lvar("@fabs", Any), [arg])
      | Cnegf -> binop Op_subf Double (Lconst("0.0", Double)) arg
      | Cload mem ->
          assert (typeof arg <> Void);
          let res = load (cast arg (Address (translate_mem mem))) in
          assert (typeof res <> Void);
          if not (is_float (typeof res))
          then cast res int_type
          else res (* TODO this has to be changed to reflect the actual type *)
      | _ -> error "Not a unary operator"
    end
  | _ -> error "There is no operator with this number of arguments"

and compile_list in_try_block exprs =
  List.map (fun x -> let tmp = helper false in_try_block x in assert (typeof tmp <> Void); cast tmp addr_type) exprs

(* returns a tuple of
 -- instructions to execute before using the result of this operation
 -- the virtual register of the result
 -- the type of the result
 *)
let compile_expr instr = helper (*true*)false false instr

let read_function phrase =
  match phrase with
  | Cfunction fd_cmm ->
      let name = fd_cmm.fun_name in
      let args = List.map (fun _ -> addr_type) fd_cmm.fun_args in
      local_functions := (translate_symbol name, args) :: !local_functions
  | Cdata _ -> ()

let compile_fundecl fd_cmm =
  label_counter := 0;
  let name = fd_cmm.fun_name in
  let args = fd_cmm.fun_args in
  let body = fd_cmm.fun_body in
  Hashtbl.clear types;
  List.iter (fun (name, args) -> Hashtbl.add types (translate_symbol name) (Function(addr_type, args))) !local_functions;
  ignore (caml_type Any body);
  let args = List.map (fun (x, typ) -> (translate_symbol (Ident.unique_name x), addr_type)) args in
  try
    let foo (x, typ) =
      let typ = try Hashtbl.find types x with Not_found -> addr_type in
      if is_int typ
      then store (cast (Lvar("%.param." ^ x, addr_type)) typ) (alloca x typ)
      else store (Lvar("%.param."^x, addr_type)) (alloca x addr_type)
    in
    let store_params = List.map foo args in
    let code = List.fold_left (@@) (Llabel "entry") store_params in
    let body = compile_expr body in
    let code = code @@ if typeof body = Void then body else return (cast body addr_type) in
    let argument_list = List.map (fun (id, _) -> "%.param." ^ id, addr_type) in
    ignore (emit_llvm (Ldefine(translate_symbol name, argument_list args, code)))
  with Llvm_error s ->
    print_endline ("error while compiling function " ^ name ^ ":");
    print_endline s;
    error s

(* vim: set foldenable : *)
