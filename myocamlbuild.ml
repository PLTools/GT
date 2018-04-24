open Printf;;
open Ocamlbuild_plugin;;

let m4_rules ext =
  let dep   = "%(name)" -.- "ml4"
  and prod1 = "%(name)" -.- "ml"
  and prod2 = "%(name: <**/*>)" -.- "ml" in
  let cppo_rule prod env _build =
    let dep = env dep in
    let prod = env prod in
    (* let tags = tags_of_pathname prod ++ "cppo" in *)
    Cmd (S[A "m4"; A"../src/macro.m4"; P dep; Sh ">"; A prod ])
  in
  rule ("cppo: *.ml4 -> *.ml")       ~dep ~prod:prod1 (cppo_rule prod1);
  rule ("cppo: **/*.ml4 -> **/*.ml") ~dep ~prod:prod2 (cppo_rule prod2)

open Command;;

let () = dispatch (fun hook ->
  Migrate_parsetree_ocamlbuild.dispatch hook;
  match hook with
  | Before_rules -> ()

  | After_rules ->
     ocaml_lib "src/GT";
     flag ["compile"; "short_paths"] & S [A "-short-paths"];

     m4_rules ();
     dep ["use_m4"] ["src/macro.m4"];
     flag ["ocaml"; "pp"; "use_pa_gt"] (S [ Sh"camlp5o camlp5/pa_gt.cmo" ]);

     flag ["ocaml"; "pp"; "use_plugins"] (S [ A"-I"; A"plugins"
                                            ; A"show.cmo";  A"gmap.cmo"
                                            ; A"foldl.cmo"; A"foldr.cmo"
                                            ; A"compare.cmo"; A"eq.cmo"
                                            ]);

     flag ["ocamldep"; "link_pa_gt"]   (S [ A"-pp"; A"camlp5o camlp5/pa_gt.cmo" ]);
     flag ["compile";  "link_pa_gt"]   (S [ A"-I";A"camlp5"; Sh"camlp5/pa_gt.cmo" ]);

     flag ["make_pp_gt"; "link"] (S[A"ppx/ppx_deriving_gt.cmxa"
                                   ;A"-package"; A"ppxlib"
                                   ]);

     dep ["compile"; "use_ppx_extension"] ["ppx/ppx_deriving_gt.cma"; "rewriter/pp_gt.native"];
     ()
 | _ -> ()
)
