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

(* The toplevel directives *)

open Format
open Misc
open Longident
open Path
open Types
open Toploop

(* The standard output formatter *)
let std_out = std_formatter

(* To quit *)

let dir_quit () = exit 0

let _ = Hashtbl.add directive_table "quit" (Directive_none dir_quit)

(* To add a directory to the load path *)

let dir_directory s =
  let d = expand_directory Config.standard_library s in
  Config.load_path := d :: !Config.load_path

let _ = Hashtbl.add directive_table "directory" (Directive_string dir_directory)

(* To show the directories on the load path *)

let dir_show_dirs () = List.iter print_endline !Config.load_path

let _ = Hashtbl.add directive_table "show_dirs" (Directive_none dir_show_dirs)

(* To change the current directory *)

let dir_cd = Sys.chdir

let _ = Hashtbl.add directive_table "cd" (Directive_string dir_cd)

(* Load in-core a .cmxs file *)

let load_file _ name = Jitlink.loadfile name; true

let dir_load _ name = Jitlink.loadfile name

let _ = Hashtbl.add directive_table "load" (Directive_string(dir_load std_out))

(* Load commands from a file *)

let dir_use ppf = function
  | "topfind" ->
      (* Special case for Findlib support, similar to the topfind.p
         initialization script that ships with Findlib. *)
      let d = Findlib.package_directory "findlib" in
      if not (List.mem d !Config.load_path) then dir_directory d;
      Topfind.reset();
      Topfind.add_predicates ["native"; "toploop"];
      Topfind.don't_load ["findlib"];
      Topfind.announce()
  | name ->
      ignore (Toploop.use_file ppf name)

let _ = Hashtbl.add directive_table "use" (Directive_string(dir_use std_out))

(* Install/remove a printer *)

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit

let match_printer_type ppf desc typename =
  let (printer_type, _) =
    try
      Env.lookup_type (Ldot(Lident "Topdirs", typename)) !toplevel_env
    with Not_found ->
      fprintf ppf "Cannot find type Topdirs.%s.@." typename;
      raise Exit in
  Ctype.init_def(Ident.current_time());
  Ctype.begin_def();
  let ty_arg = Ctype.newvar() in
  Ctype.unify !toplevel_env
    (Ctype.newconstr printer_type [ty_arg])
    (Ctype.instance desc.val_type);
  Ctype.end_def();
  Ctype.generalize ty_arg;
  ty_arg

let find_printer_type ppf lid =
  try
    let (path, desc) = Env.lookup_value lid !toplevel_env in
    let (ty_arg, is_old_style) =
      try
        (match_printer_type ppf desc "printer_type_new", false)
      with Ctype.Unify _ ->
        (match_printer_type ppf desc "printer_type_old", true) in
    (ty_arg, path, is_old_style)
  with
  | Not_found ->
      fprintf ppf "Unbound value %a.@." Printtyp.longident lid;
      raise Exit
  | Ctype.Unify _ ->
      fprintf ppf "%a has a wrong type for a printing function.@."
      Printtyp.longident lid;
      raise Exit

let dir_install_printer ppf lid =
  try
    let (ty_arg, path, is_old_style) = find_printer_type ppf lid in
    let v = eval_path path in
    let print_function =
      if is_old_style then
        (fun formatter repr -> Obj.obj v (Obj.obj repr))
      else
        (fun formatter repr -> Obj.obj v formatter (Obj.obj repr)) in
    install_printer path ty_arg print_function
  with Exit -> ()

let dir_remove_printer ppf lid =
  try
    let (ty_arg, path, is_old_style) = find_printer_type ppf lid in
    begin try
      remove_printer path
    with Not_found ->
      fprintf ppf "No printer named %a.@." Printtyp.longident lid
    end
  with Exit -> ()

let _ =
  Hashtbl.add directive_table "install_printer"
             (Directive_ident(dir_install_printer std_out));
  Hashtbl.add directive_table "remove_printer"
             (Directive_ident(dir_remove_printer std_out))

(* The trace *)

let dir_trace ppf _ =
  fprintf ppf "The #trace directive is not supported.@."

let dir_untrace ppf _ =
  fprintf ppf "The #untrace directive is not supported.@."

let dir_untrace_all ppf () =
  fprintf ppf "The #untrace_all directive is not supported.@."

let _ =
  Hashtbl.add directive_table "trace"
             (Directive_ident(dir_trace std_out));
  Hashtbl.add directive_table "untrace"
             (Directive_ident(dir_untrace std_out));
  Hashtbl.add directive_table "untrace_all"
             (Directive_none(dir_untrace_all std_out))

let parse_warnings ppf iserr s =
  try Warnings.parse_options iserr s
  with Arg.Bad err -> fprintf ppf "%s.@." err

let _ =
(* Control the printing of values *)

  Hashtbl.add directive_table "print_depth"
             (Directive_int(fun n -> max_printer_depth := n));
  Hashtbl.add directive_table "print_length"
             (Directive_int(fun n -> max_printer_steps := n));

(* Set various compiler flags *)

  Hashtbl.add directive_table "labels"
             (Directive_bool(fun b -> Clflags.classic := not b));

  Hashtbl.add directive_table "principal"
             (Directive_bool(fun b -> Clflags.principal := b));

  Hashtbl.add directive_table "warnings"
             (Directive_string(parse_warnings std_out false));

  Hashtbl.add directive_table "warn_error"
             (Directive_string(parse_warnings std_out true))
