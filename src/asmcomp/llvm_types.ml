exception Llvm_error of string

type 'a error = Just of 'a | Error of string

type llvm_type =
    Integer of int (* bitwidth *)
  | Double
  | Address of llvm_type
  | Void
  | Any
  | Function of llvm_type * llvm_type list (* return type, argument types *)

type cast = Zext | Trunc | Bitcast | Inttoptr | Ptrtoint | Sitofp | Fptosi

type binop =
    Op_addi | Op_subi | Op_muli | Op_divi | Op_modi
  | Op_and | Op_or | Op_xor | Op_lsl | Op_lsr | Op_asr
  | Op_addf | Op_subf | Op_mulf | Op_divf

type llvm_instr =
  | Lvar of string * llvm_type (* name, type *)
  | Lbinop of binop * llvm_type * llvm_instr * llvm_instr (* op, typ, left, right *)
  | Lcomp of string * llvm_type * llvm_instr * llvm_instr (* op, typ, left, right *)
  | Lalloca of string * llvm_type (*name, typ*)
  | Lload of llvm_instr (* address *)
  | Lstore of llvm_instr * llvm_instr (* value, address *)
  | Lcast of cast * llvm_instr * llvm_type * llvm_type
  | Lgetelementptr of llvm_instr * llvm_instr (* address, offset *)
  | Lcall of llvm_type * llvm_instr * llvm_instr list (* return type, name, arguments *)
  | Lccall of llvm_type * llvm_instr * llvm_instr list (* return type, name, arguments; using c calling convention *)
  | Lconst of string * llvm_type (* literal, type *)
  | Llabel of string (* name *)
  | Lbr of string (* label *)
  | Lbr_cond of llvm_instr * string * string (* condition, then label, else label *)
  | Lswitch of string * llvm_instr * int array * llvm_instr * llvm_instr array * llvm_type (* indexes, blocks *)
  | Lreturn of llvm_instr * llvm_type (* value, type *)
  | Lseq of llvm_instr * llvm_instr (* value, type *)
  | Lcaml_raise_exn of llvm_instr (* argument *)
  | Lcaml_catch_exn of string * llvm_instr * llvm_instr (* ident, what to do, where to store result *)
  | Lcaml_alloc of int (* length *)
  | Ldefine of string * (string * llvm_type) list * llvm_instr (* name, arguments, body *)
  | Lnothing
  | Lunreachable
  | Lcomment of string




let error s = raise (Llvm_error s)

let size_int = 8 * Arch.size_int
let size_float = 8 * Arch.size_float

let int_type = Integer size_int
let addr_type = Address int_type
let float_sized_int = Integer size_float

let rec string_of_type = function
  | Integer i -> "i" ^ string_of_int i
  | Double -> "double"
  | Address typ -> string_of_type typ ^ "*"
  | Void -> "void"
  | Any -> error "unable to infer type"
  | Function(ret, args) -> string_of_type ret ^ " (" ^ String.concat ", " (List.map string_of_type args) ^ ")"

let deref typ = match typ with
  | Address typ -> typ
  | _ -> error ("trying to dereference non-pointer type " ^ string_of_type typ)

let string_of_binop = function
  | Op_addi -> "add"
  | Op_subi -> "sub"
  | Op_muli -> "mul"
  | Op_divi -> "sdiv"
  | Op_modi -> "srem"
  | Op_and  -> "and"
  | Op_or   -> "or"
  | Op_xor  -> "xor"
  | Op_lsl  -> "shl"
  | Op_lsr  -> "lshr"
  | Op_asr  -> "ashr"
  | Op_addf -> "fadd"
  | Op_subf -> "fsub"
  | Op_mulf -> "fmul"
  | Op_divf -> "fdiv"

let string_of_cast = function
  | Zext -> "zext"
  | Trunc -> "trunc"
  | Bitcast -> "bitcast"
  | Inttoptr -> "inttoptr"
  | Ptrtoint -> "ptrtoint"
  | Sitofp -> "sitofp"
  | Fptosi -> "fptosi"

let rec has_type = function
  | Llabel _ | Ldefine(_,_,_) | Lunreachable | Lcomment _ | Lbr _ | Lbr_cond(_,_,_)
  | Lstore(_,_) | Lcaml_raise_exn _ -> false
  | Lseq(instr1, instr2) -> has_type instr2 || has_type instr1
  | _ -> true

let rec typeof = function
  | Lvar(_, typ) -> typ
  | Lbinop(_,typ,_,_) -> typ
  | Lcomp(_,_,_,_) -> Integer 1
  | Lalloca(_, typ) -> Address typ
  | Lload addr -> deref (typeof addr)
  | Lstore(_,_) -> Void
  | Lcast(op,_,_,typ) -> typ
  | Lgetelementptr(ptr,_) -> typeof ptr
  | Lcall(typ,_,_) -> typ
  | Lccall(typ,_,_) -> typ
  | Lconst(_,typ) -> typ
  | Llabel _ -> error "Label does not have a type"
  | Lbr _ -> error "branch does not return anything"
  | Lbr_cond(_,_,_) -> error "conditional branch does not return anything"
  | Lswitch(_,_,_,_,_,typ) -> typ
  | Lreturn(_, typ) -> typ
  | Lseq(_, instr) when has_type instr -> typeof instr
  | Lseq(instr,_) -> typeof instr
  | Lcaml_raise_exn _ -> Void
  | Lcaml_catch_exn(_,_,res) -> typeof res
  | Lcaml_alloc _ -> addr_type
  | Ldefine(_,_,_) -> error "Function..."
  | Lnothing -> Void
  | Lunreachable -> error "Lunreachable does not have a type"
  | Lcomment _ -> error "Lcomment does not have a type"

let (@@) a b = Lseq(a, b)

let return x = Just x
let fail x = Error x

let (>>=) value fn = match value with
  | Just value -> fn value
  | Error s -> fail s

let (++) a b = match a, b with
  | Just a, Just b -> return (a,b)
  | Error e, _ -> fail e
  | _, Error e -> fail e
