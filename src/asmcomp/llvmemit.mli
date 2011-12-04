exception Llvm_error of string

type 'a error = Just of 'a | Error of string

val error : string -> 'a

type llvm_type =
  | Integer of int (* bitwidth *)
  | Double
  | Address of llvm_type
  | Return_type
  | Void
  | Any
  | Function of llvm_type * llvm_type list (* return type, argument types *)

val size_int : int
val size_float : int

val int_type : llvm_type
val addr_type : llvm_type
val float_sized_int : llvm_type

val typename : llvm_type -> string

val deref : llvm_type -> llvm_type

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
  | Lswitch of string * llvm_instr * int list * llvm_instr * llvm_type (* value, type *)
  | Lreturn of llvm_instr * llvm_type (* value, type *)
  | Lseq of llvm_instr * llvm_instr (* value, type *)
  | Lcaml_raise_exn of llvm_instr (* argument *)
  | Lcaml_catch_exn of string (* TODO figure out what information is needed *)
  | Lcaml_alloc of int (* length *)
  | Ldefine of string * (string * llvm_type) list * llvm_instr (* name, arguments, body *)
  | Lnothing
  | Lunreachable
  | Lcomment of string

val typeof : llvm_instr -> llvm_type

val (@@) : llvm_instr -> llvm_instr -> llvm_instr

val llvm_instrs : llvm_instr list ref

val constants : string list ref
val functions : (string * string * string list) list ref

val add_const : string -> unit
val add_function : (llvm_type * string * (llvm_instr list)) -> unit

val emit_constant_declarations : unit -> unit
val emit_function_declarations : unit -> unit
val begin_assembly : unit -> unit
val end_assembly : unit -> unit
val emit_llvm : llvm_instr -> string error

val to_string : llvm_instr -> string
val data : Cmm.data_item list -> unit

val assemble_file : string -> string -> string -> int
