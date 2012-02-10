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
  let cast op value reg = insert (Lcast op) [|value|] reg in
  begin
    match typ, dest_typ with
    | (a, b) when a = b -> ()
    | (Integer i, Integer j) when i < j -> cast Zext value reg
    | (Integer i, Integer j) when i > j -> cast Trunc value reg
    | (Integer i, Address _) when i = size_int * 8 -> cast Inttoptr value reg
    | (Integer i, Double) when i = size_float * 8 -> cast Bitcast value reg
    | (Integer _, Function(_,_)) -> cast Bitcast value reg
    | (Double, Integer i) when i = size_float * 8 -> cast Bitcast value reg
    | (Address _, Address _) -> cast Bitcast value reg
    | (Address _, Integer _) -> cast Ptrtoint value reg
    | (Double, Address _) -> let tmp = register "tmp" float_sized_int in cast Bitcast value tmp; cast Inttoptr tmp reg
    | (Address _, Double) -> let tmp = register "tmp" float_sized_int in cast Ptrtoint value tmp; cast Bitcast tmp reg
    | (a, b) -> error ("error while trying to cast " ^ string_of_reg value ^
                       " to " ^ string_of_type dest_typ)
  end;
  if typ = dest_typ then value else reg

let cast value dest_typ = cast_reg value dest_typ (register "" dest_typ)

let alloca result = assert (typeof result <> Address Void); insert Lalloca [||] result; result
let load addr result = insert Lload [|addr|] result; result
let branch lbl = insert_simple (Lbranch lbl)
let label lbl = insert_simple (Llabel lbl)
let getelemptr addr offset res = insert Lgetelemptr [|addr; offset|] res; res

let counter = ref 0
let c () = counter := !counter + 1; "." ^ string_of_int !counter

let exits = Hashtbl.create 10

let rec last_instr instr =
  match instr.Llvm_mach.next.Llvm_mach.desc with
    Iend -> instr
  | _ -> last_instr instr.Llvm_mach.next

let caml_young_ptr = Const("@caml_young_ptr", Address addr_type)
let caml_young_limit = Const("@caml_young_limit", Address addr_type)

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
        (*
        ignore (alloca res)
        *)
        let a = alloca res in
        if is_addr (deref (typeof a)) then
          insert (Lextcall (Const("@llvm.gcroot", Function(Void, [Address (Address byte); Address byte]))))
            [|cast a (Address (Address byte)); Const("null", Address byte)|] Nothing;
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
        if typeof res = typ then
          insert Lgetelemptr [|cast addr typ; cast offset int_type|] res
        else
          let tmp_reg = register "" typ in
          insert Lgetelemptr [|cast addr typ; cast offset int_type|] tmp_reg;
          ignore (cast_reg tmp_reg (typeof res) res)
    | Icall fn, args ->
        print_debug "Icall";
        let ret = match typ with Address(Function(ret,_)) -> ret | _ -> error "not a function" in
        if typeof res = ret then
          insert (Lcall (cast fn typ)) (Array.map (fun arg -> cast arg addr_type) args) res
        else
          let tmp_reg = register "" ret in
          insert (Lcall (cast fn typ)) (Array.map (fun arg -> cast arg addr_type) args) tmp_reg;
          ignore (cast_reg tmp_reg (typeof res) res)
    | Iextcall fn, args ->
        print_debug "Iextcall";
        let arg_typ = Array.of_list (match typ with Address(Function(_, args)) -> args | _ -> error "not a function") in
        insert (Lextcall (cast fn typ)) (Array.mapi (fun i arg -> cast arg arg_typ.(i)) args) res
    | Iifthenelse(ifso, ifnot),  [|cond|] ->
        print_debug "Iifthenelse";
        assert (typeof cond = bit);
        let counter = c () in
        let then_lbl, else_lbl, endif_lbl = "then" ^ counter, "else" ^ counter, "endif" ^ counter in
        let if_res = if typeof res = Void then Nothing else (assert (typeof res <> Void); alloca (register "if_tmp" (Address (typeof res)))) in
        insert (Lcondbranch(then_lbl, else_lbl)) [|cond|] Nothing;
        label then_lbl;
        linear ifso;
        if typeof (last_instr ifso).Llvm_mach.res = Void then ()
        else insert Lstore [|cast (last_instr ifso).Llvm_mach.res (typeof res); if_res|] Nothing;
        branch endif_lbl;
        label else_lbl;
        linear ifnot;
        if typeof (last_instr ifnot).Llvm_mach.res = Void then ()
        else insert Lstore [|cast (last_instr ifnot).Llvm_mach.res (typeof res); if_res|] Nothing;
        branch endif_lbl;
        label endif_lbl;
        if typeof res = Void then ()
        else insert Lload [|if_res|] res
    | Iswitch(indexes, blocks), [|value|] ->
        print_debug "Iswitch";
        let c = c () in
        let labels = Array.map (fun i -> "case" ^ string_of_int i ^ c) indexes in
        let switch_res = alloca (register "" (assert (typ <> Address Void); if typ <> Void then Address typ else addr_type)) in
        insert (Lswitch("default" ^ c, labels)) [|cast value int_type|] Nothing;
        label ("default" ^ c);
        insert Lstore [|Const("@caml_exn_Match_failure", addr_type); Const("@caml_exn", Address addr_type)|] Nothing;
(*        insert (Lextcall (Const("@llvm.eh.sjlj.longjmp", Function(Void, [Address byte]))))
          [|cast (Const("@caml_jump_buffer", Address Jump_buffer)) (Address byte)|] Nothing;
 *)
        insert (Lextcall (Const("@longjmp", Function(Void, [Address byte; Integer 32]))))
          [|cast (Const("@caml_jump_buffer", Address Jump_buffer)) (Address byte); Const("1", Integer 32)|] Nothing;
        insert_simple Lunreachable;
        Array.iteri
          (fun i block ->
             label ("case" ^ string_of_int i ^ c);
             linear block;
             if typ <> Void then
               let res = (last_instr block).Llvm_mach.res in
               if typeof res <> Void then
               insert Lstore [|cast (last_instr block).Llvm_mach.res typ; switch_res|] Nothing;
             branch ("endswitch" ^ c);
          ) blocks;
        label ("endswitch" ^ c);
        insert Lload [|switch_res|] res
    | Ireturn, [|value|] ->
        print_debug "Ireturn";
        if Void = typ then insert Lreturn [|cast (Const("123456789", int_type)) addr_type|] Nothing
        else insert Lreturn [|cast value typ|] Nothing
    | Iunreachable, [||] ->
        print_debug "Iunreachable";
        insert_simple Lunreachable
    | Icomment s, [||] ->
        print_debug "Icomment";
        insert_simple (Lcomment s)
    | Iraise, [|exn|] ->
        print_debug "Iraise";
        insert Lstore [|cast exn addr_type; Const("@caml_exn", Address addr_type)|] Nothing;
        (*
        insert (Lextcall (Const("@llvm.eh.sjlj.longjmp", Function(Void, [Address byte]))))
          [|cast (Const("@caml_jump_buffer", Address Jump_buffer)) (Address byte)|] Nothing;
         *)
        insert (Lextcall (Const("@longjmp", Function(Void, [Address byte; Integer 32]))))
          [|cast (Const("@caml_jump_buffer", Address Jump_buffer)) (Address byte); Const("1", Integer 32)|] Nothing;
        insert_simple Lunreachable
    | Itrywith(try_instr, with_instr), [||] -> 
        print_debug "Itrywith";
        let c = c() in
        let try_lbl, with_lbl, cont_lbl = "try" ^ c, "with" ^ c, "cont" ^ c in
        (* TODO write exception handling code *)
        let old_jmp_buf = alloca (register "old_jmp_buf" (Address Jump_buffer)) in
        let temp_buf = load (Const("@caml_jump_buffer", Address Jump_buffer)) (register "" Jump_buffer) in
        insert Lstore [|temp_buf; old_jmp_buf|] Nothing;
        let set_jmp_res = register "" (Integer 32) in
        (*
        insert (Lextcall (Const("@llvm.eh.sjlj.setjmp", Function(Integer 32, [Address byte]))))
          [|cast (Const("@caml_jump_buffer", Address Jump_buffer)) (Address byte)|] set_jmp_res;
        *)
        insert (Lextcall (Const("@setjmp", Function(Integer 32, [Address byte]))))
          [|cast (Const("@caml_jump_buffer", Address Jump_buffer)) (Address byte)|] set_jmp_res;
        let tmp = if typ <> Void then alloca (register "try_with_tmp" (Address typ)) else Nothing in
        let cond = register "" bit in
        insert (Lcomp Comp_eq) [|set_jmp_res; Const("0", int_type)|] cond;
        insert (Lcondbranch(try_lbl, with_lbl)) [|cond|] Nothing;

        let try_res = (last_instr try_instr).Llvm_mach.res in
        label try_lbl;
        linear try_instr;
        if typeof try_res <> Void then insert Lstore [|cast try_res typ; tmp|] Nothing;
        let temp_buf = load old_jmp_buf (register "" Jump_buffer) in
        insert Lstore [|temp_buf; Const("@caml_jump_buffer", Address Jump_buffer)|] Nothing;
        branch cont_lbl;

        let with_res = (last_instr with_instr).Llvm_mach.res in
        label with_lbl;
        let temp_buf = load old_jmp_buf (register "" Jump_buffer) in
        insert Lstore [|temp_buf; Const("@caml_jump_buffer", Address Jump_buffer)|] Nothing;
        linear with_instr;
        if typeof with_res <> Void then insert Lstore [|cast with_res typ; tmp|] Nothing;
        branch cont_lbl;

        label cont_lbl;
        if typ <> Void then insert Lload [|tmp|] res
    | Icatch(i, body, handler), [||] ->
        let c = c () in
        Hashtbl.add exits i c;
        print_debug "Icatch";
        let tmp = if typ <> Void then alloca (register "catch_tmp" (Address typ)) else Nothing in
        linear body;
        let body_res = (last_instr body).Llvm_mach.res in
        if typeof body_res <> Void then insert Lstore [|cast body_res typ; tmp|] Nothing else insert_simple (Lcomment "nothing to store in body");
        branch ("endcatch" ^ c);
        label ("exit" ^ string_of_int i ^ c);
        Hashtbl.remove exits i;
        linear handler;
        let handler_res = (last_instr handler).Llvm_mach.res in
        if typeof handler_res <> Void then insert Lstore [|cast handler_res typ; tmp|] Nothing
        else insert_simple (Lcomment "nothing to store in handler");
        branch ("endcatch" ^ c);
        label ("endcatch" ^ c);
        if typ <> Void then insert Lload [|tmp|] res
    | Iexit i, [||] ->
        print_debug "Iexit";
        branch ("exit" ^ string_of_int i ^ Hashtbl.find exits i)
    | Ialloc len, [||] ->
        print_debug "Ialloc";
        let counter = c () in
        let collect_lbl, continue_lbl = "collect" ^ counter, "continue" ^ counter in
        insert_simple (Lcomment ("allocating " ^ string_of_int len ^ "*8 bytes"));
        let young_limit = load caml_young_limit (register "young_limit" addr_type) in
        let young_ptr = load caml_young_ptr (register "young_ptr" addr_type) in
        let nyp = getelemptr young_ptr (Const(string_of_int (-len), int_type)) (register "" (typeof young_ptr)) in
        insert Lstore [|nyp; caml_young_ptr|] Nothing;
        let cmp_res = register "enough_memory" bit in
        insert (Lcomp Comp_lt) [|nyp; young_limit|] cmp_res;
        insert (Lcondbranch(collect_lbl, continue_lbl)) [|cmp_res|] Nothing;

        label collect_lbl;
        insert_simple (Lextcall (Const("@caml_call_gc", Function(Void, []))));
        let young_ptr = load caml_young_ptr (register "young_ptr" addr_type) in
        let nyp = getelemptr young_ptr (Const(string_of_int (-len), int_type)) (register "" (typeof young_ptr)) in
        insert Lstore [|nyp; caml_young_ptr|] Nothing;
        branch continue_lbl;

        label continue_lbl;
        insert Lload [|caml_young_ptr|] res
        (*
        let alloc, args =
          match len with
            2 -> "@caml_alloc1", []
          | 3 -> "@caml_alloc2", []
          | 4 -> "@caml_alloc3", []
          | _ -> "@caml_allocN", [addr_type]
        in
        insert (Lextcall (Const(alloc, Function(addr_type, args))))
          (if len > 4 then [|Const("inttoptr(" ^ string_of_type int_type ^ " " ^ string_of_int (len-1) ^ " to " ^ string_of_type addr_type ^ ")", addr_type)|] else [||]) res
        *)

        (* TODO tell LLVM that the garbage collection is unlikely *)
    | _, _ -> error ("unknown instruction:\n" ^ Llvm_aux.to_string i)
  end; linear next end



let move_allocas_to_entry_block instr =
  let allocas = ref [] in
  let rec helper prev instr =
    match instr.desc with
    | Lend -> !allocas
    | Lalloca -> allocas := instr :: !allocas; prev.next <- instr.next; helper prev prev.next
    | _ -> helper instr instr.next
  in
  let allocas = helper instr instr.next in
  List.iter (fun i -> i.next <- instr.next; instr.next <- i) allocas;
  instr



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
      fun_body = move_allocas_to_entry_block instrs }
  with Llvm_error s ->
    print_endline ("error while linearising " ^ f.name);
    print_endline (to_string f.body);
    raise (Llvm_error s)
