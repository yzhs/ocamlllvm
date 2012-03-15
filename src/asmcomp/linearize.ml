open Arch
open Aux
open Reg
open Mach

let error s = error ("Llvm_linearize: " ^ s)

type label = string

type cast = Zext | Sext | Trunc | Bitcast | Inttoptr | Ptrtoint

type instruction =
  { mutable desc: instruction_desc;
    mutable next: instruction;
    arg: register array;
    res: register;
    dbg: Debuginfo.t }

and instruction_desc =
    Lend
  | Lcast of cast
  | Lcomp of comp 
  | Lop of binop
  | Lfptosi | Lsitofp
  | Lalloca | Lload | Lstore | Lgetelemptr
  | Lcall of register | Lextcall of register
  | Lreturn
  | Llabel of label
  | Lbranch of label
  | Lcondbranch of label * label 
  | Lswitch of label * label array
  | Lunreachable
  | Lcomment of string

type fundecl =
  { fun_name: string;
    fun_args: register list;
    fun_body: instruction }

let rec end_instr =
  { desc = Lend;
    next = end_instr;
    arg = [||];
    res = Nothing;
    dbg = Debuginfo.none }


let string_of_cast = function
    Zext -> "zext"
  | Sext -> "sext"
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
  { desc = d; next = n; arg = i.Mach.arg;
    res = i.Mach.res; dbg = i.Mach.dbg }



let instr_seq = ref []

let insert d a r =
  instr_seq := (d, a, r, Debuginfo.none) :: !instr_seq

let insert_simple d =
  instr_seq := (d, [||], Nothing, Debuginfo.none) :: !instr_seq

let reverse_instrs tmp =
  List.fold_left (fun seq (desc, arg, res, dbg) -> instr_cons_debug desc arg res dbg seq) end_instr !tmp




let rec cast_reg value dest_typ reg =
  let typ = typeof value in
  let cast op value reg = insert (Lcast op) [|value|] reg in
  begin
    match typ, dest_typ with
    | (a, b) when a = b -> ()
    | (Integer i, Integer j) when i < j -> cast Zext value reg
    | (Integer i, Integer j) when i > j -> cast Trunc value reg
    | (Integer i, Address _) when i = size_int * 8 -> cast Inttoptr value reg
    | (Integer i, Address _) -> cast Inttoptr (cast_reg value int_type (new_reg "" int_type)) reg
    | (Integer i, Double) when i = size_float * 8 -> cast Bitcast value reg
    | (Integer _, Function(_,_)) -> cast Bitcast value reg
    | (Double, Integer i) when i = size_float * 8 -> cast Bitcast value reg
    | (Address _, Address _) -> cast Bitcast value reg
    | (Address _, Integer _) -> cast Ptrtoint value reg
    | (Double, Address _) -> let tmp = new_reg "tmp" float_sized_int in cast Bitcast value tmp; cast Inttoptr tmp reg
    | (Address _, Double) -> let tmp = new_reg "tmp" float_sized_int in cast Ptrtoint value tmp; cast Bitcast tmp reg
    | (a, b) -> error ("error while trying to cast " ^ string_of_reg value ^
                       " to " ^ string_of_type dest_typ)
  end;
  if typ = dest_typ then value else reg

let cast value dest_typ = cast_reg value dest_typ (new_reg "" dest_typ)

let allocas : (string, instruction) Hashtbl.t = Hashtbl.create 10

let alloca result =
  assert (typeof result <> Address Void);
  if Hashtbl.mem allocas (reg_name result) then begin
    insert_simple (Lcomment "stripped an alloca, using the one with the same name already existing");
    Const(reg_name result, typeof (Hashtbl.find allocas (reg_name result)).res)
  end else begin
    Hashtbl.add allocas (reg_name result) {desc = Lalloca; next = end_instr; arg = [||]; res = result; dbg = Debuginfo.none};
    if is_addr (deref (typeof result)) then
      insert (Lextcall (global "llvm.gcroot" (function_type Void [Address (Address byte); Address byte])))
        [|cast result (Address (Address byte)); const "null" (Address byte)|] Nothing;
    result
  end

let load addr result = insert Lload [|addr|] result; result
let branch lbl = insert_simple (Lbranch lbl)
let label lbl = insert_simple (Llabel lbl)
let getelemptr addr offset res = insert Lgetelemptr [|addr; offset|] res; res



let counter = ref 0
let c () = counter := !counter + 1; "." ^ string_of_int !counter

let exits = Hashtbl.create 10

let rec last_instr instr =
  match instr.Mach.next.Mach.desc with
    Iend -> instr
  | _ -> last_instr instr.Mach.next

let block_res instr = (last_instr instr).Mach.res

let current_function = ref ""

let rec linear i =
  let { Mach.desc = desc; Mach.next = next; Mach.arg = arg;
        Mach.res = res; Mach.typ = typ; Mach.dbg = dbg } = i in
  if desc <> Iend then begin begin
    match desc, arg with
      Iend, [||] -> error "this case should never be reached"
    | Ibinop op, [|left; right|] ->
        insert (Lop op) [|cast left typ; cast right typ|] res
    | Icomp op, [|left; right|] ->
        insert (Lcomp op) [|cast left typ; cast right typ|] res
    | Ialloca, [||] -> ignore (alloca res)
    | Iload, [|addr|] -> ignore (load (cast addr (Address typ)) res)
    | Istore, [|value; addr|] ->
        insert Lstore [|cast value typ; cast addr (Address typ)|] Nothing
    | Ifptosi, [|value|] -> insert Lfptosi [|cast value Double|] res
    | Isitofp, [|value|] -> insert Lsitofp [|cast value int_type|] res
    | Izext, [|value|] -> insert (Lcast Zext) [|(*no cast necessary*)value|] res
    | Isext, [|value|] -> insert (Lcast Sext) [|(*no cast necessary*)value|] res
    | Igetelementptr, [|addr; offset|] ->
        if typeof res = typ then
          insert Lgetelemptr [|cast addr typ; cast offset int_type|] res
        else
          let tmp_reg = new_reg "" typ in
          insert Lgetelemptr [|cast addr typ; cast offset int_type|] tmp_reg;
          ignore (cast_reg tmp_reg (typeof res) res)
    | Icall fn, args ->
        let ret = ret_type typ in
        if typeof res = ret then
          insert (Lcall (cast fn typ)) (Array.map (fun arg -> cast arg addr_type) args) res
        else
          let tmp_reg = new_reg "" ret in
          insert (Lcall (cast fn typ)) (Array.map (fun arg -> cast arg addr_type) args) tmp_reg;
          ignore (cast_reg tmp_reg (typeof res) res)
    | Iextcall(fn, alloc), args ->
        let arg_types = Array.of_list (arg_types typ) in
        let insert_call () =
          insert (Lextcall (cast fn typ)) (Array.mapi (fun i arg -> cast arg arg_types.(i)) args) res
        in
        if alloc then
          let c = c () in
          let dummy_lbl, ret_lbl = "dummy" ^ c, "ret" ^ c in
          insert Lstore [|const ("blockaddress(" ^ !current_function ^ ", %" ^ ret_lbl ^ ")") (Address byte);
                          global "caml_last_return_address" (Address(Address byte))|] Nothing;
          let stackpointer = new_reg "sp" (Address byte) in
          insert (Lextcall (global "llvm.stacksave" (function_type byte []))) [||] stackpointer;
          insert Lstore [|stackpointer; global "caml_bottom_of_stack" (Address (Address byte))|] Nothing;
          insert_call ();
          branch ret_lbl;
          label ret_lbl
        else
          insert_call ()
    | Iifthenelse(ifso, ifnot),  [|cond|] ->
        assert (typeof cond = bit);
        let counter = c () in
        let then_lbl, else_lbl, endif_lbl = "then" ^ counter, "else" ^ counter, "endif" ^ counter in
        let if_res = if typeof res = Void then Nothing else (assert (typeof res <> Void); alloca (new_reg "if_tmp" (Address (typeof res)))) in
        insert (Lcondbranch(then_lbl, else_lbl)) [|cond|] Nothing;

        label then_lbl;
        linear ifso;
        let ifso_res = block_res ifso in
        if typeof ifso_res <> Void
        then insert Lstore [|cast (block_res ifso) (typeof res); cast if_res (Address (typeof res))|] Nothing;
        branch endif_lbl;

        label else_lbl;
        linear ifnot;
        let ifnot_res = block_res ifnot in
        if typeof ifnot_res <> Void
        then insert Lstore [|cast ifnot_res (typeof res); cast if_res (Address (typeof res))|] Nothing;
        branch endif_lbl;

        label endif_lbl;
        if typeof res <> Void then insert Lload [|cast if_res (Address (typeof res))|] res
    | Iswitch(indexes, blocks), [|value|] ->
        let c = c () in
        let labels = Array.map (fun i -> "case" ^ string_of_int i ^ c) indexes in
        let switch_res = alloca (new_reg "" (assert (typ <> Address Void); if typ <> Void then Address typ else addr_type)) in
        insert (Lswitch("default" ^ c, labels)) [|cast value int_type|] Nothing;
        label ("default" ^ c);
        insert Lstore [|global "caml_exn_Match_failure" addr_type; caml_exn|] Nothing;
(*        insert (Lextcall (Const("@llvm.eh.sjlj.longjmp", Function(Void, [Address byte]))))
          [|cast jmp_buf (Address byte)|] Nothing;
 *)
        insert (Lextcall longjmp) [|cast jmp_buf (Address byte); const "1" (Integer 32)|] Nothing;
        insert_simple Lunreachable;
        Array.iteri
          (fun i block ->
             label ("case" ^ string_of_int i ^ c);
             linear block;
             if typ <> Void then begin
               let res = block_res block in
               if typeof res <> Void then
                 insert Lstore [|cast res typ; switch_res|] Nothing
             end;
             branch ("endswitch" ^ c))
          blocks;
        label ("endswitch" ^ c);
        insert Lload [|switch_res|] res
    | Ireturn, [|value|] ->
        if Void = typ then insert Lreturn [|const "undef" addr_type|] Nothing
        else insert Lreturn [|cast value typ|] Nothing
    | Iunreachable, [||] ->
        insert_simple Lunreachable
    | Icomment s, [||] ->
        insert_simple (Lcomment s)
    | Iraise, [|exn|] ->
        insert Lstore [|cast exn addr_type; caml_exn|] Nothing;
        (*
        insert (Lextcall (Const("@llvm.eh.sjlj.longjmp", Function(Void, [Address byte]))))
          [|cast jmp_buf (Address byte)|] Nothing;
         *)
        insert (Lextcall (global "longjmp" (function_type Void [Address byte; Integer 32])))
          [|cast jmp_buf (Address byte);
            const "1" (Integer 32)|] Nothing;
        insert_simple Lunreachable
    | Itrywith(try_instr, with_instr), [||] -> 
        let c = c() in
        let try_lbl, with_lbl, cont_lbl = "try" ^ c, "with" ^ c, "cont" ^ c in
        let old_jmp_buf = alloca (new_reg "old_jmp_buf" (Address Jump_buffer)) in
        let temp_buf = load jmp_buf (new_reg "" Jump_buffer) in
        insert Lstore [|temp_buf; old_jmp_buf|] Nothing;
        let set_jmp_res = new_reg "" (Integer 32) in
        insert (Lextcall setjmp) [|cast jmp_buf (Address byte)|] set_jmp_res;
        let tmp = if typ <> Void then alloca (new_reg "try_with_tmp" (Address typ)) else Nothing in
        let cond = new_reg "" bit in
        insert (Lcomp Comp_eq) [|set_jmp_res; Const("0", int_type)|] cond;
        insert (Lcondbranch(try_lbl, with_lbl)) [|cond|] Nothing;

        label try_lbl;
        linear try_instr;
        let try_res = block_res try_instr in
        if typeof try_res <> Void then insert Lstore [|cast try_res typ; tmp|] Nothing;
        let temp_buf = load old_jmp_buf (new_reg "" Jump_buffer) in
        insert Lstore [|temp_buf; jmp_buf|] Nothing;
        branch cont_lbl;

        let with_res = (last_instr with_instr).Mach.res in
        label with_lbl;
        let temp_buf = load old_jmp_buf (new_reg "" Jump_buffer) in
        insert Lstore [|temp_buf; jmp_buf|] Nothing;
        linear with_instr;
        if typeof with_res <> Void then insert Lstore [|cast with_res typ; tmp|] Nothing;
        branch cont_lbl;

        label cont_lbl;
        if typ <> Void then insert Lload [|tmp|] res
    | Icatch(i, body, handler), [||] ->
        let c = c () in
        Hashtbl.add exits i c;
        let tmp = if typ <> Void then alloca (new_reg "catch_tmp" (Address typ)) else Nothing in
        linear body;
        let body_res = (last_instr body).Mach.res in
        if typeof body_res <> Void then insert Lstore [|cast body_res typ; tmp|] Nothing
        else insert_simple (Lcomment "nothing to store in body");
        branch ("endcatch" ^ c);

        label ("exit" ^ string_of_int i ^ c);
        Hashtbl.remove exits i;
        linear handler;
        let handler_res = (last_instr handler).Mach.res in
        if typeof handler_res <> Void then insert Lstore [|cast handler_res typ; tmp|] Nothing
        else insert_simple (Lcomment "nothing to store in handler");
        branch ("endcatch" ^ c);

        label ("endcatch" ^ c);
        if typ <> Void then insert Lload [|tmp|] res
    | Iloop body, [||] ->
        let lbl = "loop" ^ c() in
        branch lbl;

        label lbl;
        linear body;
        branch lbl
    | Iexit i, [||] ->
        branch ("exit" ^ string_of_int i ^ Hashtbl.find exits i)
    | Ialloc len, [||] ->

        let counter = c () in
        let collect_lbl, continue_lbl = "collect" ^ counter, "continue" ^ counter in
        insert_simple (Lcomment ("allocating " ^ string_of_int len ^ "*8 bytes"));
        let young_limit = load caml_young_limit (new_reg "young_limit" addr_type) in
        let young_ptr = load caml_young_ptr (new_reg "young_ptr" addr_type) in
        let nyp = getelemptr young_ptr (Const(string_of_int (-len), int_type)) (new_reg "" (typeof young_ptr)) in
        insert Lstore [|nyp; caml_young_ptr|] Nothing;
        let cmp_res = new_reg "enough_memory" bit in
        insert (Lcomp Comp_lt) [|nyp; young_limit|] cmp_res;
        insert (Lcondbranch(collect_lbl, continue_lbl)) [|cmp_res|] Nothing;

        label collect_lbl;
        insert_simple (Lextcall(Const("@caml_call_gc", Function(Void, []))));
        let young_ptr = load caml_young_ptr (new_reg "young_ptr" addr_type) in
        let nyp = getelemptr young_ptr (Const(string_of_int (-len), int_type)) (new_reg "" (typeof young_ptr)) in
        insert Lstore [|nyp; caml_young_ptr|] Nothing;
        branch continue_lbl;

        label continue_lbl;
        insert Lload [|caml_young_ptr|] res

        (*
        let alloc, arg_types, args =
          match len with
            2 -> "@caml_alloc1", [], [||]
          | 3 -> "@caml_alloc2", [], [||]
          | 4 -> "@caml_alloc3", [], [||]
          | _ ->
              "@caml_allocN", [addr_type],
              [|Const("inttoptr(" ^ string_of_type int_type ^ " " ^
                      string_of_int (len-1) ^ " to " ^ string_of_type addr_type ^
                      ")", addr_type)|]
        in
        insert (Lextcall (Const(alloc, Function(addr_type, arg_types)))) args res
         *)

        (* TODO tell LLVM that the garbage collection is unlikely *)
    | _, _ -> error ("unknown instruction:\n" ^ Printmach.instr_to_string i)
  end; linear next end


let rec len instr =
  match instr.desc with
    Lend -> 0
  | _ -> 1 + len instr.next


let insert_allocas allocas instrs =
  let instr = ref instrs.next in
  let insert_alloca a =
    a.next <- !instr;
    instr := a;
  in
  List.iter insert_alloca allocas;
  !instr

let get_allocas () =
  (* Hashtbl.fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c *)
  let result = Hashtbl.fold (fun _ b c -> b :: c) allocas [] in
  Hashtbl.clear allocas;
  result


let fundecl f =
  current_function := "@" ^ f.name;
  try
    counter := 0;
    label "entry";
    print_debug ("linearising " ^ f.name);
    print_debug (Printmach.instrs_to_string f.body);
    linear f.body;
    let instrs = reverse_instrs instr_seq in
    instr_seq := [];
    { fun_name = f.name;
      fun_args = List.map (fun (name, typ) -> Reg(name, typ)) f.args;
      fun_body = insert_allocas (get_allocas()) instrs }
  with Llvm_error s ->
    print_endline ("error while linearising " ^ f.name);
    print_endline (Printmach.instrs_to_string f.body);
    raise (Llvm_error s)
