let () = Ppxlib.Driver.standalone ()
(* let () = Ppxlib.Driver.run_as_ppx_rewriter () *)

(*
(cd _build && OCAMLPATH=/home/kakadu/asp/gt-ppx/_build/bundle ocamlfind ocamlopt -predicates ppx_driver -linkpkg -package GT.ppx -package ppxlib.runner -o asdf)
*)
