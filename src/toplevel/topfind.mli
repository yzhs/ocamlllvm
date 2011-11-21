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

val predicates: string list ref
  (* The list of predicates used for package loading. *)

val add_predicates: string list -> unit
  (* Adds predicates to the list of predicates. *)

val syntax: string -> unit
  (* Emulates the [-syntax] option. *)

val standard_syntax: unit -> unit
  (* Adds predicates that select the standard syntax. Same as
     [syntax "camlp4o"]. *)

val revised_syntax: unit -> unit
  (* Adds predicates that select the revised syntax. Same as
     [syntax "camlp4r"]. *)

val don't_load: string list -> unit
  (* The packages named in pkglist are added to the list of packages which
     are already loaded. *)

val don't_load_deeply: string list -> unit
  (* The packages named in pkglist and all direct and indirect ancestors
     are added to the list of packages which are already loaded. *)

val load: string list -> unit
  (* The packages from the passed package list are loaded, from left to
     right, but packages that have already been loaded are left out. *)

val load_deeply: string list -> unit
  (* The packages from the passed package list and all direct or indirect
     ancestors are loaded in topological order. Packages that have already
     been loaded are left out. *)

val reset: unit -> unit
  (* All entries in the list of loaded packages that have been added by
     [load] or [load_deeply] functions are removed from this list. This
     means that if you execute the same [load] or [load_deeply] functions
     again, the packages will be reloaded. *)

val announce: unit -> unit
  (* Output the startup message. *)
