(***********************************************************************)
(*                                                                     *)
(*                              ocamlnat                               *)
(*                                                                     *)
(*                  Benedikt Meurer, University of Siegen              *)
(*                                                                     *)
(*    Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,    *)
(*    Universität Siegen. All rights reserved. This file is distri-    *)
(*    buted under the terms of the Q Public License version 1.0.       *)
(*                                                                     *)
(***********************************************************************)

(* Command-line parameters *)

let objfiles = ref ([] : string list)   (* .cmo and .cma files *)
and ccobjs = ref ([] : string list)     (* .o, .a, .so and -cclib -lxxx *)
and dllibs = ref ([] : string list)     (* .so and -dllib -lxxx *)

let compile_only = ref false            (* -c *)
and output_name = ref (None : string option) (* -o *)
and include_dirs = ref ([] : string list)(* -I *)
and no_std_include = ref false          (* -nostdlib *)
and print_types = ref false             (* -i *)
and make_archive = ref false            (* -a *)
and debug = ref false                   (* -g *)
and fast = ref false                    (* -unsafe *)
and link_everything = ref false         (* -linkall *)
and custom_runtime = ref false          (* -custom *)
and output_c_object = ref false         (* -output-obj *)
and ccopts = ref ([] : string list)     (* -ccopt *)
and classic = ref false                 (* -nolabels *)
and nopervasives = ref false            (* -nopervasives *)
and preprocessor = ref(None : string option) (* -pp *)
let annotations = ref false             (* -annot *)
and use_threads = ref false             (* -thread *)
and use_vmthreads = ref false           (* -vmthread *)
and noassert = ref false                (* -noassert *)
and verbose = ref false                 (* -verbose *)
and noprompt = ref false                (* -noprompt *)
and init_file = ref (None : string option)   (* -init *)
and use_prims = ref ""                  (* -use-prims ... *)
and use_runtime = ref ""                (* -use-runtime ... *)
and principal = ref false               (* -principal *)
and recursive_types = ref false         (* -rectypes *)
and strict_sequence = ref false         (* -strict-sequence *)
and applicative_functors = ref true     (* -no-app-funct *)
and make_runtime = ref false            (* -make_runtime *)
and gprofile = ref false                (* -p *)
and c_compiler = ref (None: string option) (* -cc *)
and no_auto_link = ref false            (* -noautolink *)
and dllpaths = ref ([] : string list)   (* -dllpath *)
and make_package = ref false            (* -pack *)
and for_package = ref (None: string option) (* -for-pack *)
let dump_parsetree = ref false          (* -dparsetree *)
and dump_rawlambda = ref false          (* -drawlambda *)
and dump_lambda = ref false             (* -dlambda *)
and dump_instr = ref false              (* -dinstr *)

let keep_asm_file = ref false           (* -S *)
let optimize_for_speed = ref true       (* -compact *)
let register_allocator = ref Config.default_register_allocator (* -regalloc *)

and dump_cmm = ref false                (* -dcmm *)
let keep_startup_file = ref false       (* -dstartup *)

let native_code = ref false             (* set to true under ocamlopt *)
let inline_threshold = ref 10

let dont_write_files = ref false        (* set to true under ocamldoc *)

let std_include_flag prefix =
  if !no_std_include then ""
  else (prefix ^ (Filename.quote Config.standard_library))
;;

let std_include_dir () =
  if !no_std_include then [] else [Config.standard_library]
;;

let shared = ref false (* -shared *)
let dlcode = ref true (* not -nodynlink *)
