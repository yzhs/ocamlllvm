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

(* Load packages from toploops and scripts *)

open Format
open Misc
open Toploop

(* Utility functions *)

let rec uniq = function
    x :: l when List.mem x l -> uniq l
  | x :: l -> x :: uniq l
  | l -> l

let split_in_words s =
  let l = String.length s in
  let rec split i j =
    if j < l then
      let j' = succ j in
      match s.[j] with
        (' '|'\t'|'\n'|'\r'|',') ->
          let rem = split j' j' in
          if i < j then (String.sub s i (j - i)) :: rem else rem
      | _ -> split i j'
    else
      if i < j then [String.sub s i (j-i)] else []
  in
  split 0 0

let normalize_dirname d =
  (* Converts the file name of the directory [d] to the normal form.
   * For Unix, the '/' characters at the end are removed, and multiple
   * '/' are deleted.
   * For Windows, all '/' characters are converted to '\'. Two
   * backslashes at the beginning are tolerated.
   *)
  let s = String.copy d in
  let l = String.length d in
  let norm_dir_unix() =
    for k = 1 to l - 1 do
      if s.[k] = '/' && s.[k-1] = '/' then s.[k] <- Char.chr 0;
      if s.[k] = '/' && k = l-1 then s.[k] <- Char.chr 0
    done
  in
  let norm_dir_win() =
    if l >= 1 && s.[0] = '/' then s.[0] <- '\\';
    if l >= 2 && s.[1] = '/' then s.[1] <- '\\';
    for k = 2 to l - 1 do
      if s.[k] = '/' then s.[k] <- '\\';
      if s.[k] = '\\' && s.[k-1] = '\\' then s.[k] <- Char.chr 0;
      if s.[k] = '\\' && k = l-1 then s.[k] <- Char.chr 0
    done
  in
  let expunge() =
    let n = ref 0 in
    for k = 0 to l - 1 do
      if s.[k] = Char.chr 0 then incr n
    done;
    let s' = String.create (l - !n) in
    n := 0;
    for k = 0 to l - 1 do
      if s.[k] <> Char.chr 0 then begin
	s'.[ !n ] <- s.[k];
	incr n
      end
    done;
    s'
  in
  match Sys.os_type with
      "Unix" | "Cygwin" -> norm_dir_unix(); expunge()
    | "Win32" -> norm_dir_win(); expunge()
    | _ -> failwith "This os_type is not supported"

(* Topfind emulation *)

let real_toploop = !Sys.interactive

let forbidden = ref ([] : string list)
let loaded = ref ([] : string list)
let predicates = ref ([] : string list)

let add_directory d =
  let d = normalize_dirname d in
  let d = expand_directory Config.standard_library d in
  if not (List.mem d !Config.load_path) then begin
    Config.load_path := d :: !Config.load_path;
    if real_toploop then
      fprintf std_formatter "@[`%s' added to search path@]@." d
  end

let add_predicates pl =
  predicates := uniq (pl @ !predicates)

let syntax s =
  add_predicates ["syntax"; s]

let standard_syntax() =
  syntax "camlp4o"

let revised_syntax() =
  syntax "camlp4r"

let load pl =
  List.iter
    (fun p ->
       if not (List.mem p !loaded) then begin
         (* Determine the package directory *)
         let d = Findlib.package_directory p in
         add_directory d;
         (* Leave package out if forbidden *)
         if not (List.mem p !forbidden) then begin
           (* Determine the 'archive' property *)
           let archive = (try Findlib.package_property !predicates p "archive"
                          with Not_found -> "") in
           (* Split the 'archive' property and load the files *)
           let archives = split_in_words archive in
           List.iter
             (fun arch ->
                let arch = Findlib.resolve_path ~base:d arch in
                Jitlink.loadfile arch;
                if real_toploop then
                  fprintf std_formatter "@[`%s' loaded@]@." arch)
             archives
         end;
         (* The package is loaded *)
         loaded := p :: !loaded
       end)
    pl

let load_deeply pl =
  (* Load the sorted list of ancestors in turn *)
  load (Findlib.package_deep_ancestors !predicates pl)

let don't_load pl =
  forbidden := uniq (pl @ !forbidden);
  (* Check if packages exist *)
  List.iter (fun p -> ignore (Findlib.package_directory p)) pl

let don't_load_deeply pl =
  (* Check if packages exist *)
  List.iter (fun p -> ignore (Findlib.package_directory p)) pl;
  (* Add the sorted list of ancestors to the forbidden packages *)
  don't_load (Findlib.package_deep_ancestors !predicates pl)

let protect f x =
  try ignore(f x)
  with Failure s -> fprintf err_formatter "@[%s@]@." s

let reset() =
  loaded := [];
  (* Add "#require" directive *)
  Hashtbl.replace
    directive_table
    "require"
    (Directive_string(protect (fun s -> load_deeply (split_in_words s))));
  (* Add "#predicates" directive *)
  Hashtbl.replace
    directive_table
    "predicates"
    (Directive_string(protect (fun s -> add_predicates (split_in_words s))));
  (* Add "#camlp4o" directive *)
  Hashtbl.replace
    directive_table
    "camlp4o"
    (Directive_none(protect (fun () ->
                               standard_syntax();
                               load_deeply ["camlp4"])));
  (* Add "#camlp4r" directive *)
  Hashtbl.replace
    directive_table
    "camlp4r"
    (Directive_none(protect (fun () ->
                               revised_syntax();
                               load_deeply ["camlp4"])));
  (* Add "#list" directive *)
  Hashtbl.replace
    directive_table
    "list"
    (Directive_none(protect (fun () ->
                               Findlib.list_packages stdout;
                               flush stdout)));
  (* Add "#thread" directive *)
  Hashtbl.replace
    directive_table
    "thread"
    (Directive_none(protect (fun () -> (*TODO*)())))

let announce() =
  if real_toploop then begin
    print_endline
      ("Findlib has been successfully loaded. Additional directives:\n" ^
       "  #require \"package\";;      to load a package\n" ^
       "  #list;;                   to list the available packages\n" ^
       "  #camlp4o;;                to load camlp4 (standard syntax)\n" ^
       "  #camlp4r;;                to load camlp4 (revised syntax)\n" ^
       "  #predicates \"p,q,...\";;   to set these predicates\n" ^
       "  Topfind.reset();;         to force that packages will be reloaded\n")
  end
