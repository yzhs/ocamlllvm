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

(* Error report *)

open Format

(* Report an error *)

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, l) ->
      Location.print_error ppf l;
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Env.Error err ->
      Location.print_error_cur_file ppf;
      Env.report_error ppf err
  | Ctype.Tags(l, l') ->
      Location.print_error_cur_file ppf;
      fprintf ppf
      "In this program,@ variant constructors@ `%s and `%s@ \
       have the same hash value.@ Change one of them." l l'
  | Typecore.Error(loc, err) ->
      Location.print_error ppf loc; Typecore.report_error ppf err
  | Typetexp.Error(loc, err) ->
      Location.print_error ppf loc; Typetexp.report_error ppf err
  | Typedecl.Error(loc, err) ->
      Location.print_error ppf loc; Typedecl.report_error ppf err
  | Typeclass.Error(loc, err) ->
      Location.print_error ppf loc; Typeclass.report_error ppf err
  | Includemod.Error err ->
      Location.print_error_cur_file ppf;
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print_error ppf loc; Typemod.report_error ppf err
  | Translcore.Error(loc, err) ->
      Location.print_error ppf loc; Translcore.report_error ppf err
  | Translclass.Error(loc, err) ->
      Location.print_error ppf loc; Translclass.report_error ppf err
  | Translmod.Error(loc, err) ->
      Location.print_error ppf loc; Translmod.report_error ppf err
  | Compilenv.Error code ->
      Location.print_error_cur_file ppf;
      Compilenv.report_error ppf code
  | Jitlink.Error err ->
      Location.print_error_cur_file ppf;
      Jitlink.report_error ppf err
  | Findlib.No_such_package(pkg, reason) ->
      fprintf ppf "No such package: %s" pkg;
      if reason <> "" then fprintf ppf " - %s" reason
  | Findlib.Package_loop pkg ->
      fprintf ppf "Package requires itself: %s" pkg
  | Sys_error msg ->
      Location.print_error_cur_file ppf;
      fprintf ppf "I/O error: %s" msg
  | Warnings.Errors (n) ->
      Location.print_error_cur_file ppf;
      fprintf ppf "Error-enabled warnings (%d occurrences)" n
  | x -> fprintf ppf "@]"; raise x in

  fprintf ppf "@[%a@]@." report exn
