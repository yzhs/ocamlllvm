open Cmm
open Emitaux
open Llvmemit

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
  | Caddi -> "add"
  | Csubi -> "sub"
  | Cmuli -> "mul"
  | Cdivi -> "sdiv"
  | Cmodi -> "srem"
  | Cand  -> "and"
  | Cor   -> "or"
  | Cxor  -> "xor"
  | Clsl  -> "shl"
  | Clsr  -> "lshr"
  | Casr  -> "ashr"
  | Caddf -> "fadd"
  | Csubf -> "fsub"
  | Cmulf -> "fmul"
  | Cdivf -> "fdiv"
  | _ -> error "not a binary operator"

let translate_mem = function
  | Byte_unsigned | Byte_signed -> Integer 8
  | Sixteen_unsigned | Sixteen_signed -> Integer 16
  | Thirtytwo_unsigned | Thirtytwo_signed -> Integer 32 
  | Word -> int_type
  | Single | Cmm.Double | Double_u -> Double

let translate_fcomp = function
  | Ceq -> "oeq"
  | Cne -> "one"
  | Clt -> "olt"
  | Cle -> "ole"
  | Cgt -> "ogt"
  | Cge -> "oge"

let translate_icomp = function
  | Ceq -> "eq"
  | Cne -> "ne"
  | Clt -> "slt"
  | Cle -> "sle"
  | Cgt -> "sgt"
  | Cge -> "sge"

let translate_ucomp = function
  | Ceq -> "eq"
  | Cne -> "ne"
  | Clt -> "ult"
  | Cle -> "ule"
  | Cgt -> "ugt"
  | Cge -> "uge"

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

let translate_machtype = function
  | [| Addr |] -> addr_type
  | [| Int |] -> addr_type
  | [| Float |] -> addr_type
  | _ -> error "unknown type"
(* }}} *)

(* {{{ *)
let rec cmm_to_string = function
  | Cconst_int i        -> "(int " ^ string_of_int i ^ ")"
  | Cconst_natint i     -> "(int " ^ Nativeint.to_string i ^ ")"
  | Cconst_float f      -> "(float " ^ f ^ ")"
  | Cconst_symbol s     -> "(symb " ^ s ^ ")"
  | Cconst_pointer i    -> "(ptr " ^ string_of_int i ^ ")"
  | Cconst_natpointer i -> "(ptr " ^ Nativeint.to_string i ^ ")"
  | Cvar id -> "(Cvar " ^ translate_symbol (Ident.unique_name id) ^ ")"
  | Clet(id,arg,body) -> "(let " ^ translate_symbol (Ident.unique_name id) ^ " = " ^ cmm_to_string arg ^ " in " ^ cmm_to_string body ^ ")"
  | Cassign(id,expr) -> "(assign " ^ translate_symbol (Ident.unique_name id) ^ " " ^ cmm_to_string expr ^ ")"
  | Ctuple [] -> "()"
  | Ctuple exprs -> "(some tuple...)"
  | Cop(Capply(typ, debug), exprs) -> "(call " ^ String.concat " " (List.map cmm_to_string exprs) ^ ")"
  | Cop(Cextcall(fn, typ, alloc, debug), exprs) -> "(extcall " ^ String.concat " " (List.map cmm_to_string exprs) ^ ")"
  | Cop(Calloc, arg) -> "(alloc " ^ String.concat " " (List.map cmm_to_string arg) ^ ")"
  | Cop(Cstore mem, [addr; value]) -> "(store " ^ typename (translate_mem mem) ^ " " ^ cmm_to_string value ^ " " ^ cmm_to_string addr ^ ")"
  | Cop(Craise debug, args) -> "(raise " ^ String.concat " " (List.map cmm_to_string args) ^ ")"
  | Cop(Ccheckbound debug, [arr; index]) -> "(checkbound " ^ cmm_to_string arr ^ " " ^ cmm_to_string index ^ ")"
  | Cop(Ccheckbound _, _) -> error "not implemented: checkound with #args != 2"
  | Cop((Caddi|Csubi|Cmuli|Cdivi|Cmodi|Cand|Cor|Cxor|Clsl|Clsr|Casr), [left;right]) -> "(op " ^ cmm_to_string left ^ " " ^ cmm_to_string right ^ ")"
  | Cop((Caddf|Csubf|Cmulf|Cdivf), [left;right]) -> "(op " ^ cmm_to_string left ^ " " ^ cmm_to_string right ^ ")"
  | Cop((Cadda|Csuba), [left;right]) -> "(op " ^ cmm_to_string left ^ " " ^ cmm_to_string right ^ ")"
  | Cop(Ccmpi op, [left;right]) -> "(cmpi " ^ cmm_to_string left ^ " " ^ cmm_to_string right ^ ")"
  | Cop(Ccmpf op, [left;right]) -> "(cmpf " ^ cmm_to_string left ^ " " ^ cmm_to_string right ^ ")"
  | Cop(Ccmpa op, [left;right]) -> "(cmpa " ^ cmm_to_string left ^ " " ^ cmm_to_string right ^ ")"
  | Cop(Cfloatofint, [arg]) -> "(floatofint " ^ cmm_to_string arg ^ ")"
  | Cop(Cintoffloat, [arg]) -> "(intoffloat " ^ cmm_to_string arg ^ ")"
  | Cop(Cabsf, [arg]) -> "(absf " ^ cmm_to_string arg ^ ")"
  | Cop(Cnegf, [arg]) -> "(negf " ^ cmm_to_string arg ^ ")"
  | Cop(Cload mem, [arg]) -> "(load " ^ typename (translate_mem mem) ^ " " ^ cmm_to_string arg ^ ")"
  | Cop(_,_) -> error "operation not available"
  | Csequence(fst,snd) -> "(seq " ^ cmm_to_string fst ^ " " ^ cmm_to_string snd ^ ")"
  | Cifthenelse(cond, expr1, expr2) -> "(if " ^ cmm_to_string cond ^ " " ^ cmm_to_string expr1 ^ " " ^ cmm_to_string expr2 ^ ")"
  | Cswitch(expr,is,exprs) -> "(switch ...)"
  | Cloop expr -> "(loop " ^ cmm_to_string expr ^ ")"
  | Ccatch(i,ids,expr1,expr2) -> "(catch ...)"
  | Cexit(i,exprs) -> "(exit ...)"
  | Ctrywith(try_expr, id, with_expr) -> "(try " ^ cmm_to_string try_expr ^ " with " ^ translate_symbol (Ident.unique_name id) ^ " " ^ cmm_to_string with_expr ^ ")"

let cast value dest_typ =
  let typ = typeof value in
  match typ, dest_typ with
  | (Integer i, Integer j) ->
      if i == j
      then value
      else if i < j
           then Lzext(value, typ, dest_typ)
           else Ltrunc(value, typ, dest_typ)
  | (Integer i, Address _) ->
      if i == size_int
      then Linttoptr(value, typ, dest_typ)
      else error ("could not cast integer of size " ^ string_of_int i ^ " to pointer")
  | (Integer i, Double) ->
      if i == size_float
      then Lbitcast(value, typ, dest_typ)
      else error ("could not cast integer of size " ^ string_of_int i ^ " to pointer")
  | (Integer _, Function(_,_)) -> Linttoptr(value, typ, dest_typ)
  | (Integer _, _) -> error ("invalid cast from integer to " ^ typename dest_typ)
  | (Double, Integer i) ->
      if i == size_float
      then Lbitcast(value, typ, dest_typ)
      else error ("could not cast float to integer of size " ^ string_of_int i)
  | (Address i, Address j) -> if i == j then value else Lbitcast(value, typ, dest_typ)
  | (Address _, Integer i) -> Lptrtoint(value, typ, dest_typ)
  | (a, b) ->
      if a == b
      then value
      else error ("error while trying to cast " ^ typename typ ^ " to " ^ typename dest_typ)

let alloca name typ = Hashtbl.add vars name (); Hashtbl.add types name (Address typ); Lalloca(name, typ)

let const x y = x

let load addr =
  Lload (cast addr (if is_addr (typeof addr)
                    then typeof addr
                    else ((*Printf.printf "Error: not an address\n";*) Address (typeof addr))))

let local_load addr typ =
  Lload (Lvar (addr, typ))

let store value addr =
  Lstore(cast value (deref (typeof addr)), addr)

let store_non_void arg addr =
  if typeof arg == Void then arg else store arg addr

let load_if_necessary typ value =
  (*Printf.printf "loading value %s if necessary\n" (to_string value);*)
  if typ == typeof value
  then ((*Printf.printf "nothing to load, typ == typeof value where value=%s\n" (to_string value);*) value)
  else if typ == Address (typeof value)
       then ((*Printf.printf "loading value=%s\n" (to_string value);*) load value)
       else ((*Printf.printf "casting %s from %s to %s\n" (to_string value) (typename (typeof value)) (typename typ);*) cast value typ)
let binop op typ left right = Lbinop(op, typ, load_if_necessary typ left, load_if_necessary typ right)
let comp op typ left right =
  (*Printf.printf "comparing %s with %s as %s\n" (typename (typeof left))
   * (typename (typeof right)) (typename typ);*)
  Lcomp(op, typ, load_if_necessary typ left, load_if_necessary typ right)

let getelementptr addr offset =
  Lgetelementptr(const addr addr_type, offset)

let load_exn_ptr () = local_load "%exn_ptr" (Address addr_type)
let load_young_ptr () = local_load "%young_ptr" (Address addr_type)

let call fn args =
  Lcall(Return_type, fn, load_exn_ptr () :: load_young_ptr () :: args)

let ccall typ fn arg_list = Lccall(typ, fn, arg_list)

let voidcall fn args = Lcall(Void, fn, load_exn_ptr () :: load_young_ptr () :: args)

let return value = Lreturn(const value addr_type, addr_type)
(* }}} *)

let rec caml_type expect = function
  | Cconst_int _        -> int_type
  | Cconst_natint _     -> int_type
  | Cconst_float _      -> Double
  | Cconst_symbol _     -> addr_type
  | Cconst_pointer _    -> addr_type
  | Cconst_natpointer _ -> addr_type

  | Cvar id ->
      let name = Ident.unique_name id in
      if expect != Any && not (Hashtbl.mem types name)
      then begin
        Printf.printf "found identifier %s :: %s\n" name (typename expect);
        Hashtbl.add types (translate_symbol (Ident.unique_name id)) expect
      end; expect
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
      ignore (caml_type int_type left); ignore (caml_type int_type right); int_type
  | Cop((Caddf|Csubf|Cmulf|Cdivf), [left;right]) ->
      ignore (caml_type Double left); ignore (caml_type Double right); Double
  | Cop((Cadda|Csuba), [left;right]) ->
      ignore (caml_type addr_type left); ignore (caml_type addr_type right); addr_type
  | Cop(Ccmpi op, [left;right]) ->
      ignore (caml_type int_type left); ignore (caml_type int_type right); int_type
  | Cop(Ccmpf op, [left;right]) ->
      ignore (caml_type Double left); ignore (caml_type Double right); int_type
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
  | Cifthenelse(cond, expr1, expr2) -> ignore (caml_type int_type cond); let typ = caml_type Any expr1 in caml_type typ expr2
  | Cswitch(expr,is,exprs) -> expect (* TODO figure out the real type *)

  | Cloop expr -> ignore (caml_type Any expr); Void
  | Ccatch(i,ids,expr1,expr2) -> Void (* TODO figure out what the real type would be *)
  | Cexit(i,exprs) -> Void (* TODO process exprs *)
  | Ctrywith(try_expr, id, with_expr) ->
      ignore (caml_type expect try_expr);
      Hashtbl.add types (translate_symbol (Ident.unique_name id)) addr_type; (* the exception's type *)
      caml_type expect with_expr


(* returns a tuple of
 -- instructions to execute before using the result of this operation
 -- the virtual register of the result
 -- the type of the result
 *)
let rec compile_expr instr = match instr with
  | Cconst_int i        -> Lconst(string_of_int i, int_type)
  | Cconst_natint i     -> Lconst(Nativeint.to_string i, int_type)
  | Cconst_float f      -> Lconst(f, Double)
  | Cconst_symbol s     -> add_const s; Lconst("@" ^ translate_symbol s, addr_type)
  | Cconst_pointer i    -> cast (Lconst(string_of_int i, int_type)) addr_type
  | Cconst_natpointer i -> cast (Lconst(Nativeint.to_string i, int_type)) addr_type

  | Cvar id -> begin
      let name = translate_symbol (Ident.unique_name id) in
      let typ = try Hashtbl.find types name
                with Not_found -> error ("Could not find identifier " ^ name ^ ".") in
      local_load ("%" ^ name) typ
    end
  | Clet(id,arg,body) ->
      let name = translate_symbol (Ident.unique_name id) in
      let res_arg = compile_expr arg in
      let type_arg = typeof res_arg in
      let typ = if Double == type_arg then Double else int_type in
      let res = if is_def (translate_symbol name) then res_arg else alloca (translate_symbol name) typ in
      store res_arg res
      @@ compile_expr body
  | Cassign(id,expr) ->
      let res = compile_expr expr in
      let name = translate_symbol (Ident.unique_name id) in
      let mem_typ = try Hashtbl.find types name with Not_found -> error ("not found: " ^ name) in
      store res (Lvar("%" ^ name, mem_typ)) (* TODO handle assignments to global variables *)
  | Ctuple [] -> Lconst(";", Void)
  | Ctuple exprs -> begin
      (* TODO What is Ctuple used for? Implement that. *)
      Lconst(";tuple_res", Void)
    end

  | Cop(Capply(typ, debug), exprs) -> begin
      match exprs with
      | Cconst_symbol s :: res ->
          let results = compile_list res in
          add_function (Return_type, translate_symbol s, Lnothing :: Lnothing :: results);
          call (Lvar("@" ^ translate_symbol s, Any)) results
      | ptr :: res ->
          let args = compile_list res in
          let fn = cast (compile_expr ptr) (Function(Return_type, addr_type :: addr_type :: (List.map (fun x -> addr_type) args))) in
          call fn args
      | [] -> error "no function specified"
    end
  | Cop(Cextcall(fn, typ, alloc, debug), exprs) ->
      let args = compile_list exprs in
      add_function (addr_type, fn, args);
      ccall addr_type (Lvar("@" ^ fn, Any)) args
  | Cop(Calloc, args) ->
      let new_young_ptr_instr = Lcaml_alloc (List.length args) in (* TODO figure out how much space a single element needs *)
      let new_young_ptr = load_young_ptr () in 
      let header = getelementptr new_young_ptr (Lconst("1", int_type)) in
      let args = List.map compile_expr args in
      let num = ref (-1) in
      let emit_arg x =
        num := !num + 1;
        let num = string_of_int !num in
        let elemptr = getelementptr header (Lconst(num, int_type)) in
        store x elemptr
      in
      List.fold_left (fun a b -> a @@ emit_arg b) new_young_ptr_instr args
      @@ getelementptr header (Lconst("1", int_type))

  | Cop(Cstore mem, [addr; value]) ->
      let addr = compile_expr addr in
      let value = compile_expr value in
      if typeof value == Address (translate_mem mem)
      then store value (cast addr (typeof value))
      else store (cast value (translate_mem mem)) addr
  | Cop(Craise debug, [arg]) ->
      Lcaml_raise_exn (cast (compile_expr arg) addr_type)
  | Cop(Craise _, _) -> error "wrong number of arguments for Craise"
  | Cop(Ccheckbound debug, [arr; index]) ->
      let arr = compile_expr arr in
      let index = compile_expr index in
      let header = getelementptr (cast arr addr_type) (Lconst("-" ^ string_of_int Arch.size_addr, int_type)) in
      let length = load header in
      (* TODO replace the following by code that actually does the right thing *)
      let cond = comp "icmp ule" (typeof length) index length in
      let c = c () in
      add_function (Void, "caml_ml_array_bound_error", [Lnothing; Lnothing]);
      Lbr_cond(cond, "out_of_bounds" ^ c, "ok" ^ c)
      @@ Lblock ("out_of_bounds" ^ c,
                 voidcall (Lvar("@caml_ml_array_bound_error",Any)) []
                 @@ Lbr ("ok" ^ c))
      @@ Lbegin_block ("ok" ^ c)
  | Cop(Ccheckbound _, _) -> error "not implemented: checkound with #args != 2"
  | Cop(op, exprs) -> compile_operation op exprs

  | Csequence(fst,snd) -> compile_expr fst @@ compile_expr snd
  | Cifthenelse(cond, expr1, expr2) -> begin
      let c = c () in
      let cond = compile_expr cond in
      let then_res = compile_expr expr1 in
      let else_res = compile_expr expr2 in
      let typ = if typeof then_res != Void then typeof then_res else typeof else_res in
      let cond = comp "icmp ne" int_type (Lconst("0", int_type)) (if typeof cond == int_type then cond else load cond) in
      let res_alloc = alloca ("if_res" ^ c) (if typ != Void then typ else int_type) in (* if typ == Void, if_res is never used *)
      let if_res = Lvar("%if_res" ^ c, Address (if typ != Void then typ else int_type)) in
      res_alloc
      @@ Lifthenelse(cond,
                     store_non_void then_res if_res,
                     store_non_void else_res if_res,
                     if typ != Void then local_load ("%if_res" ^ c) (Address typ) else Lnothing)
    end
  | Cswitch(expr,indexes,exprs) ->
      let indexes = Array.to_list indexes in
      let exprs = List.map compile_expr (Array.to_list exprs) in
      let c = c () in
      (* TODO implement switch *)
      let typ = try typeof (List.find (fun x -> typeof x != Void) exprs) with Not_found -> Void in
      let value = alloca ("switch_res" ^ c) typ @@ compile_expr expr in
      let blocks =
        List.map (fun (i, expr) -> Lcase ("case" ^ string_of_int i ^ "." ^ c, expr))
                 (List.combine indexes exprs) in
      Lswitch(c, value, indexes, blocks, typ)
  | Cloop expr ->
      let c = c () in
      Lbr ("loop" ^ c)
      @@ Lblock ("loop" ^ c, compile_expr expr @@ Lbr ("loop" ^ c))
  | Ccatch(i,ids,expr1,expr2) ->
      compile_expr expr1
      @@ Lbr ("exit" ^ string_of_int i)
      @@ Lblock ("exit" ^ string_of_int i, compile_expr expr2)
  | Cexit(i,exprs) ->
      List.fold_left (fun lst expr -> compile_expr expr @@ lst) Lnothing exprs
      @@ Lbr ("exit" ^ string_of_int i)
  | Ctrywith(try_expr, id, with_expr) ->
      let counter = c () in
      let res = compile_expr try_expr in
      let with_res = compile_expr with_expr in
      let typ = if typeof res != Void then typeof res else typeof with_res in
      let try_with_res = Lvar("%try_with_res" ^ counter, if typ == Void then int_type else Address typ) in
      (*Hashtbl.add types (translate_symbol (Ident.unique_name id)) addr_type;*)
      (* TODO generate usable code here *)
      let trywith =
        alloca ("try_with_res" ^ counter) typ
        @@ store_non_void res try_with_res
        @@ alloca (translate_symbol (Ident.unique_name id)) int_type (* TODO replace by type appropriate for exceptions *)
        @@ store_non_void with_res try_with_res
      in if typ == Void then trywith else trywith @@ local_load ("%try_with_res" ^ counter) (Address typ)

and compile_operation op exprs =
  match exprs with
  | [l;r] -> begin
      let left = compile_expr l in
      let right = compile_expr r in
      match op with
      | Caddi | Csubi | Cmuli | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl | Clsr | Casr ->
          binop (translate_op op) int_type left right
      | Caddf | Csubf | Cmulf | Cdivf ->
          binop (translate_op op) Double left right
      | Ccmpi op ->
          cast (comp ("icmp " ^ translate_icomp op) int_type left right) int_type
      | Ccmpf op ->
          cast (comp ("fcmp " ^ translate_fcomp op) Double left right) int_type
      | Ccmpa op ->
          cast (comp ("icmp " ^ translate_ucomp op) int_type left right) int_type
      | Cadda | Csuba ->
           cast (binop "add" int_type (cast left int_type) (cast right int_type)) addr_type
      | _ -> error "Not a binary operator"
      end

  | [arg] -> begin
      let res = compile_expr arg in
      match op with
      | Cfloatofint -> Lsitofp(res, Integer size_float, Double)
      | Cintoffloat -> Lfptosi(res, Double, Integer size_float)
      | Cabsf -> ccall Double (Lvar("@fabs",Any)) [const res Double]
      | Cnegf -> binop "fsub" Double (Lconst("0.0", Double)) res
      | Cload mem ->
          let res = load (cast res (Address (translate_mem mem))) in
          if not (is_float (typeof res))
          then cast res int_type
          else res (* TODO this has to be changed to reflect the actual type *)
      | _ -> error "wrong op"
    end
  | _ -> error "There is no operator with this number of arguments"

and compile_list exprs =
  List.map (fun x -> cast (compile_expr x) addr_type) exprs

let argument_list = List.map (fun (id, _) -> "%.param." ^ id, addr_type)

let compile_fundecl fd_cmm =
  label_counter := 0;
  let name = fd_cmm.fun_name in
  let args = fd_cmm.fun_args in
  let body = fd_cmm.fun_body in
  Printf.printf "compiling %s\n" name;
  Hashtbl.clear types;
  ignore (caml_type Any body);
  let args = ("exn_ptr", addr_type) :: ("young_ptr", addr_type )
             :: List.map (fun (x, typ) -> (translate_symbol (Ident.unique_name x), translate_machtype typ)) args
  in try
     let store_params =
       List.map (fun (x, typ) ->
                   let typ = try Hashtbl.find types x with Not_found -> addr_type in
                   if is_int typ
                   then store (cast (Lvar("%.param." ^ x, addr_type)) int_type) (alloca x int_type)
                   else store (Lvar("%.param."^x, typ)) (alloca x typ)) args in
     let code = List.fold_left (@@) (Lblock ("entry", Lnothing)) store_params in
     let code = code @@ return (cast (compile_expr body) addr_type) in
     ignore (emit_llvm (Ldefine(name, argument_list args, code)))
  with Llvm_error s -> print_endline s;
                       emit_constant_declarations ();
                       emit_function_declarations ();
                       error s

let data d =
  emit_function_declarations ();
  emit_constant_declarations ();
  functions := [];
  constants := [];
  Llvmemit.data d


(* vim: set foldenable : *)
