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

open OUnit

let ocamlnat = ref "ocamlnat"

let readfile fn =
  let ic = open_in fn in
  begin try
    let n = in_channel_length ic in
    let b = Buffer.create n in
    Buffer.add_channel b ic n;
    close_in ic;
    b
  with exn ->
    close_in ic;
    raise exn
  end

let test_basic options dirname filename =
  filename >:: begin fun () ->
    let path = Filename.concat dirname filename in
    let reference = readfile (path ^ ".reference") in
    let result = Buffer.create (Buffer.length reference) in
    assert_command
      ~foutput:(Stream.iter (Buffer.add_char result))
      ~use_stderr:true
      !ocamlnat
      (options @ [path ^ ".ml"]);
    assert_equal result reference
  end

let suite_basic dirname =
  let tests = List.map
                (fun fn -> Filename.chop_suffix fn ".reference")
                  (List.filter
                    (fun fn -> Filename.check_suffix fn ".reference")
                    (Array.to_list (Sys.readdir dirname))) in
  dirname >:::
  [
    "safe/gc" >:::
      (List.map (test_basic ["-regalloc"; "gc"] dirname) tests);
    "unsafe/gc" >:::
      (List.map (test_basic ["-regalloc"; "gc"; "-unsafe"] dirname) tests);
    "safe/ls" >:::
      (List.map (test_basic ["-regalloc"; "ls"] dirname) tests);
    "unsafe/ls" >:::
      (List.map (test_basic ["-regalloc"; "ls"; "-unsafe"] dirname) tests);
  ]

let suite =
  "ocamlnat" >:::
  [
    suite_basic "basic";
  ]

let _ =
  run_test_tt_main
    ~arg_specs:["-ocamlnat",
                Arg.String (fun fn -> ocamlnat := fn),
                "fn Path to ocamlnat"]
    suite

