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

(* Start the [ocamlnat] toplevel loop *)

open Clflags

let usage = "Usage: ocamlnat <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref ([] : string list)

let prepare ppf =
  Toploop.set_paths ();
  try
    let res =
      List.for_all (Topdirs.load_file ppf) (List.rev !preload_objects) in
    !Toploop.toplevel_startup_hook ();
    res
  with x ->
    try Toperrors.report_error ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmxs"
  then preload_objects := name :: !preload_objects
  else
    begin
      let newargs = Array.sub Sys.argv !Arg.current
                                       (Array.length Sys.argv - !Arg.current)
      in
      if prepare ppf && Toploop.run_script ppf name newargs
      then exit 0
      else exit 2
    end

let regalloc_argument name =
  let ppf = Format.err_formatter in
  match name with
    "gc" | "ls" as name ->
      Clflags.register_allocator := name
  | name ->
      Format.fprintf ppf "Unsupported register allocator: %s\n" name;
      exit 2

let print_config () =
  Config.print_config stdout;
  exit 0

let print_version () =
  Printf.printf "The OCaml native toplevel, version %s\n" Config.version;
  exit 0

module Options = struct
  let list =
  [
    "-compact",
      Arg.Clear optimize_for_speed,
      " Optimize code size rather than speed";
    "-config",
      Arg.Unit print_config,
      " Print configuration values and exit";
    "-I",
      Arg.String (fun dir ->
                    let dir = Misc.expand_directory
                                Config.standard_library
                                dir in
                    include_dirs := dir :: !include_dirs),
      "<dir>  Add <dir> to the list of include directories";
    "-init",
      Arg.String (fun s -> init_file := Some s),
      "<file>  Load <file> instead of default init file";
    "-inline",
      Arg.Int (fun n -> inline_threshold := n * 8),
      "<n>  Set aggressiveness of inline to <n>";
    "-labels",
      Arg.Clear classic,
      " Use commuting label mode";
    "-no-app-funct",
      Arg.Clear applicative_functors,
      " Deactivate applicative functors";
    "-noassert",
      Arg.Set noassert,
      " Do not compile assertion checks";
    "-nolabels",
      Arg.Set classic,
      " Ignore non-optional labels in types";
    "-noprompt",
      Arg.Set noprompt,
      " Suppress all prompts";
    "-nostdlib",
      Arg.Set no_std_include,
      " Do not add default directory to the list of include directories";
    "-principal",
      Arg.Set principal,
      " Check principality of type inference";
    "-rectypes",
      Arg.Set recursive_types,
      " Allow arbitrary recursive types";
    "-regalloc",
      Arg.String regalloc_argument,
      Printf.sprintf
      "<name>  Use register allocator <name>:\n\
      \        gc   Use the graph coloring register allocator.\n\
      \        ls   Use the linear scan register allocator.\n\
      \     Default setting is %S." Config.default_register_allocator;
    "-strict-sequence",
      Arg.Set strict_sequence,
      " Left-hand part of a sequence must have type unit";
    "-unsafe",
      Arg.Set fast,
      " Do not compile bounds checking on array and string access";
    "-version",
      Arg.Unit print_version,
      " Print version and exit";
    "-w",
      Arg.String (Warnings.parse_options false),
      Printf.sprintf
      "<list>  Enable or disable warnings according to <list>:\n\
      \        +<spec>   enable warnings in <spec>\n\
      \        -<spec>   disable warnings in <spec>\n\
      \        @<spec>   enable warnings in <spec> and treat them as errors\n\
      \     <spec> can be:\n\
      \        <num>             a single warning number\n\
      \        <num1>..<num2>    a range of consecutive warning numbers\n\
      \        <letter>          a predefined set\n\
      \     default setting is %S" Warnings.defaults_w;
    "-warn-error",
      Arg.String (Warnings.parse_options true),
      Printf.sprintf
      "<list>  Enable or disable error status for warnings according\n\
      \     to <list>.  See option -w for the syntax of <list>.\n\
      \     Default setting is %S" Warnings.defaults_warn_error;
    "-warn-help",
      Arg.Unit Warnings.help_warnings,
      " Show description of warning numbers";

    "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
    "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
    "-dlambda", Arg.Set dump_lambda, " (undocumented)";
    "-dcmm", Arg.Set dump_cmm, " (undocumented)";
    "-dsel", Arg.Set dump_selection, " (undocumented)";
    "-dcombine", Arg.Set dump_combine, " (undocumented)";
    "-dlive", Arg.Unit (fun () ->
                          dump_live := true;
                          Printmach.print_live := true), " (undocumented)";
    "-dspill", Arg.Set dump_spill, " (undocumented)";
    "-dsplit", Arg.Set dump_split, " (undocumented)";
    "-dinterf", Arg.Set dump_interf, " (undocumented)";
    "-dprefer", Arg.Set dump_prefer, " (undocumented)";
    "-dinterval", Arg.Set dump_interval, " (undocumented)";
    "-dalloc", Arg.Set dump_regalloc, " (undocumented)";
    "-dreload", Arg.Set dump_reload, " (undocumented)";
    "-dscheduling", Arg.Set dump_scheduling, " (undocumented)";
    "-dlinear", Arg.Set dump_linear, " (undocumented)";

    "-",
      Arg.String (file_argument),
      "<file>  Treat <file> as a file name (even if it starts with `-')"
  ];;
end;;

let main () =
  Arg.parse Options.list file_argument usage;
  if not (prepare Format.err_formatter) then exit 2;
  Toploop.loop Format.std_formatter
