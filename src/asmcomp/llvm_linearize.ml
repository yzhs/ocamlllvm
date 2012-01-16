open Llvm_aux
open Llvm_mach

let error s = Llvm_mach.error ("Llvm_linearize: " ^ s)

type label = string

type cast = Zext | Trunc | Bitcast | Inttoptr | Ptrtoint

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: ssa_reg array;
    res: ssa_reg;
    dbg: Debuginfo.t }

and instruction_desc =
    Lend
  | Lcast of cast
  | Lcomp of comp 
  | Lop of binop
  | Lfptosi | Lsitofp
  | Lalloca | Lload | Lstore | Lgetelemptr
  | Lcall of ssa_reg | Lextcall of ssa_reg
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of label * label 
  | Lswitch of label * label array
  | Lunreachable
  | Lcomment of string

type fundecl =
  { fun_name: string;
    fun_args: ssa_reg list;
    fun_body: instruction }

let rec end_instr =
  { desc = Lend;
    next = end_instr;
    arg = [||];
    res = Nothing;
    dbg = Debuginfo.none }


let string_of_cast = function
    Zext -> "zext"
  | Trunc -> "trunc"
  | Bitcast -> "bitcast"
  | Inttoptr -> "inttoptr"
  | Ptrtoint -> "ptrtoint"

(* Cons an instruction (debug empty) *)
let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r; dbg = Debuginfo.none }

(* Cons an instruction (debug empty) *)
let instr_cons_debug d a r dbg n =
  { desc = d; next = n; arg = a; res = r; dbg = dbg }

(* Cons a simple instruction (arg, res, debug empty) *)
let cons_instr d n =
  { desc = d; next = n; arg = [||]; res = Nothing; dbg = Debuginfo.none }

(* Build an instruction with arg, res, dbg, live taken from
   the given Mach.instruction *)
let copy_instr d i n =
  { desc = d; next = n; arg = i.Llvm_mach.arg;
    res = i.Llvm_mach.res; dbg = i.Llvm_mach.dbg }

let instr_seq = ref []

let insert d a r =
  instr_seq := (d, a, r, Debuginfo.none) :: !instr_seq

let insert_simple d =
  instr_seq := (d, [||], Nothing, Debuginfo.none) :: !instr_seq

let reverse_instrs tmp =
  List.fold_left (fun seq (desc, arg, res, dbg) -> instr_cons_debug desc arg res dbg seq) end_instr !tmp

let register str typ = Llvm_mach.register str typ

let cast_reg value dest_typ reg =
  let typ = typeof value in
  if typ = dest_typ then value else begin
  let cast op value = insert (Lcast op) [|value|] reg in
  begin
    match typ, dest_typ with
    | (Integer i, Integer j) ->
        if i < j then cast Zext value
        else cast Trunc value
    | (Integer i, Address _) ->
        if i == size_int * 8 then cast Inttoptr value
        else error ("could not cast integer of size " ^ string_of_int i ^ " to pointer")
    | (Integer i, Double) ->
        if i == size_float * 8 then cast Bitcast value
        else error ("could not cast integer of size " ^ string_of_int i ^ " to pointer")
    | (Integer _, Function(_,_)) -> cast Bitcast value
    | (Integer _, _) -> error ("invalid cast from integer to " ^ string_of_type dest_typ)
    | (Double, Integer i) ->
        if i == size_float * 8 then cast Bitcast value
        else error ("could not cast float to integer of size " ^ string_of_int i)
    | (Address _, Address _) -> cast Bitcast value
    | (Address _, Integer _) -> cast Ptrtoint value
    | (Double, Address _) -> error "invalid cast: Double -> Address _"
    | (a, b) -> error ("error while trying to cast " ^ string_of_type typ ^
                       " to " ^ string_of_type dest_typ);
  end;
  reg
  end

let cast value dest_typ = cast_reg value dest_typ (register "" dest_typ)

let alloca result = insert Lalloca [||] result; result
let load addr result = insert Lload [|addr|] result; result
let branch lbl = insert_simple (Lbranch lbl)
let label lbl = insert_simple (Llabel lbl)
let getelemptr addr offset res = insert Lgetelemptr [|addr; offset|] res; res

let counter = ref 0
let c () = counter := !counter + 1; string_of_int !counter

let rec last_instr instr =
  match instr.Llvm_mach.next.Llvm_mach.desc with
    Iend -> instr
  | _ -> last_instr instr.Llvm_mach.next

(* TODO emit appropriate casts *)
let rec linear i =
  let { Llvm_mach.desc = desc; Llvm_mach.next = next; Llvm_mach.arg = arg;
                         Llvm_mach.res = res; Llvm_mach.typ = typ; Llvm_mach.dbg = dbg } = i in
  if desc <> Iend then begin begin
    match desc, arg with
      Iend, [||] -> error "this case should never be reached"
    | Ibinop op, [|left; right|] ->
        print_debug "Ibinop";
        insert (Lop op) [|cast left typ; cast right typ|] res
    | Icomp op, [|left; right|] ->
        print_debug "Icomp";
        insert (Lcomp op) [|cast left typ; cast right typ|] res
    | Ialloca, [||] ->
        print_debug "Ialloca";
        ignore (alloca res)
    (*
     let arg = cast (Iconst("%" ^ name, Address typ)) (Address byte_ptr) in
     if is_addr typ then
     ignore (call "ccc" Void (Ivar("@llvm.gcroot", Function(Void, [Address byte_ptr; byte_ptr])))
     [arg; Iconst("null", byte_ptr)]);
     *)
    | Iload, [|addr|] ->
        print_debug "Iload";
        ignore (load (cast addr (Address typ)) res)
    | Istore, [|value; addr|] ->
        print_debug "Istore";
        insert Lstore [|cast value typ; cast addr (Address typ)|] Nothing
    | Ifptosi, [|value|] ->
        print_debug "Ifptosi";
        insert Lfptosi [|cast value Double|] res
    | Isitofp, [|value|] ->
        print_debug "Isitofp";
        insert Lsitofp [|cast value int_type|] res
    | Igetelementptr, [|addr; offset|] ->
        print_debug "Igetelementptr";
        let tmp_reg = register "" typ in
        insert Lgetelemptr [|cast addr typ; cast offset int_type|] tmp_reg;
        ignore (cast_reg tmp_reg (typeof res) res)
    | Icall fn, args ->
        print_debug "Icall";
        insert (Lcall (cast fn typ)) (Array.map (fun arg -> cast arg addr_type) args) res
    | Iextcall fn, args ->
        print_debug "Iextcall";
        insert (Lextcall (cast fn typ)) (Array.map (fun arg -> cast arg addr_type) args) res
    | Iifthenelse(ifso, ifnot),  [|cond|] ->
        print_debug "Iifthenelse";
        assert (typeof cond = bit);
        let then_lbl = "then" ^ c() in
        let else_lbl = "else" ^ c() in
        let endif_lbl = "endif" ^ c() in
        let if_res = if typeof res = Void then Nothing else alloca (register "if_tmp" (Address (typeof res))) in
        insert (Lcondbranch(then_lbl, else_lbl)) [|cond|] Nothing;
        label then_lbl;
        linear ifso;
        if typeof (last_instr ifso).Llvm_mach.res = Void then ()
        else insert Lstore [|(last_instr ifso).Llvm_mach.res; if_res|] Nothing;
        branch endif_lbl;
        label else_lbl;
        linear ifnot;
        if typeof (last_instr ifnot).Llvm_mach.res = Void then ()
        else insert Lstore [|(last_instr ifnot).Llvm_mach.res; if_res|] Nothing;
        branch endif_lbl;
        label endif_lbl;
        if typeof res = Void then ()
        else insert Lload [|if_res|] res
    | Iswitch(indexes, blocks), [|value|] ->
        print_debug "Iswitch";
        ()
        (*
        let value_reg = register "" int_type in
        n := linear next !n;
        Array.iter (fun block -> n := linear block !n) blocks;
        n := instr_cons (Lswitch(default, labels)) (* TODO is this correct? *)
               [|value_reg|] Nothing !n;
        cast value value_reg !n
         *)
    | Ireturn, [|value|] ->
        print_debug "Ireturn";
        insert Lreturn [|cast value typ|] Nothing
    | Iunreachable, [||] ->
        print_debug "Iunreachable";
        insert_simple Lunreachable
    | Icomment s, [||] ->
        print_debug "Icomment";
        insert_simple (Lcomment s)
    | Iraise, [|exn|] ->
        print_debug "Iraise";
        insert Lstore [|cast exn addr_type; Const("@exn", Address addr_type)|] Nothing;
        insert (Lextcall (Const("@llvm.eh.sjlj.longjmp", Function(Void, [Address byte])))) [|cast (Const("@jmp_buf", Address Jump_buffer)) (Address byte)|] Nothing;
    | Itrywith(try_instr, with_instr), [||] -> 
        print_debug "Itrywith";
        let old_jmp_buf = alloca (register "old_jmp_buf" (Address Jump_buffer)) in
        let temp_buf = load (cast (Const("@jmp_buf", Address Jump_buffer)) (Address byte)) (register "temp_buf" Jump_buffer) in
        insert Lstore [|temp_buf; old_jmp_buf|] Nothing;
        ignore (load (Const("@exn", addr_type)) res)
    | Icatch(i, instr1, instr2), [||] ->
        print_debug "Icatch";
        ()
    | Iexit i, [||] ->
        print_debug "Iexit";
        ()
    | Ialloc len, [||] ->
        print_debug "Ialloc";
        let begin_lbl, collect_lbl, continue_lbl = "begin" ^ c(), "collect" ^ c(), "continue" ^ c() in
        insert_simple (Lcomment ("allocating " ^ string_of_int len ^ "*8 bytes"));
        branch begin_lbl;
        label begin_lbl;
        let young_limit = load (Const("@caml_young_limit", Address addr_type)) (register "young_limit" addr_type) in
        let young_ptr = load (Const("@caml_young_ptr", Address addr_type)) (register "young_ptr" addr_type) in
        let nyp = getelemptr young_ptr (Const(string_of_int (-len), int_type)) (register "" (typeof young_ptr)) in
        let cmp_res = register "enough_memory" bit in
        insert (Lcomp Comp_lt) [|nyp; young_limit|] cmp_res;
        insert (Lcondbranch(collect_lbl, continue_lbl)) [|cmp_res|] Nothing;
        label collect_lbl;
        insert_simple (Lextcall (Const("@caml_call_gc", Function(Void, []))));
        branch begin_lbl;
        label continue_lbl;
        insert Lstore [|nyp; Const("@caml_young_ptr", Address addr_type)|] Nothing;
        insert (Lcast Bitcast) [|nyp|] res
        (*
        if len = 2 then Iccall(addr_type, Ivar("@caml_alloc1", Any), [])
        else if len = 3 then Iccall(addr_type, Ivar("@caml_alloc2", Any), [])
        else if len = 4 then Iccall(addr_type, Ivar("@caml_alloc3", Any), [])
        else Iccall(addr_type, Ivar("@caml_allocN", Any), [Icast(Inttoptr, Iconst(string_of_int (len-1), int_type), int_type, addr_type)])
         *)
        (* TODO rewrite the code so it does not create a loop *)
        (* TODO tell LLVM that the garbage collection is unlikely *)
    | _, _ -> error ("unknown instruction:\n" ^ Llvm_aux.to_string i)
  end; linear next end
(*
      @@ Istore(addr_type, Icast(Igetelementptr(young_ptr, Iconst(offset, int_type)), typeof young_ptr, addr_type), Ialloca(new_young, addr_type))
 *)

let rec len instr =
  match instr.desc with
    Lend -> 0
  | _ -> 1 + len instr.next

let fundecl f =
  try
    counter := 0;
    label "entry";
    print_debug ("linearising " ^ f.name);
    print_debug (to_string f.body);
    linear f.body;
    let instrs = reverse_instrs instr_seq in
    instr_seq := [];
    { fun_name = f.name;
      fun_args = List.map (fun (name, typ) -> Reg(name, typ)) f.args;
      fun_body = instrs }
  with Llvm_error s ->
    print_endline ("error while linearising " ^ f.name);
    print_endline (to_string f.body);
    raise (Llvm_error s)
