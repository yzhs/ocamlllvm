open Ocamlbuild_plugin

let _ = 
(* The version number *)
rule "sys.ml"
  ~prod:"sys.ml"
  ~deps:["sys.mlp"; "VERSION"]
  begin fun _ _ ->
    let version = with_input_file "VERSION" input_line in
    Seq [rm_f "sys.ml";
         Cmd (S[A"sed"; A"-e";
                A(Printf.sprintf "s,%%%%VERSION%%%%,%s," version);
                Sh"<"; P"sys.mlp"; Sh">"; Px"sys.ml"]);
         chmod (A"-w") "sys.ml"]
  end;;

(* We are building the standard library so the old one is not needed *)
flag ["ocaml"; "compile"] (A "-nostdlib")
