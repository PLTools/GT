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

let make_plugins_args ~is_byte =
  (* N.B. Order matters *)
  let names = [ "show"; "gmap"; "foldl"; "foldr"; "compare"; "eq"; "html" ] in
  List.map (fun s -> A(Printf.sprintf "plugins/%s.cm%s" s (if is_byte then "o" else "x")) )
    names

let () = dispatch (fun hook ->
  match hook with
  | Before_rules -> ()
  | After_rules ->
    ocaml_lib "common/GTCommon";
    ocaml_lib "src/GT";
    (* flag ["compile"; "short_paths"] & S [A "-short-paths"]; *)

    m4_rules ();
    dep ["use_m4"] ["src/macro.m4"];
    flag ["ocaml"; "pp"; "use_pa_gt"] (S [ Sh"../camlp5o_pp.sh" ]);
    flag ["ocaml"; "link"; "link_pagtcmo"] (S [ A"camlp5/pa_gt.cma" ]);
    flag ["ocaml"; "link"; "link_pp5gt"]
      (S[ A"-package"; A"ppxlib"
        ; A"common/GTCommon.cma"
        ; A"camlp5/pa_gt.cma"
        ]);

     (* flag ["ocaml"; "pp"; "use_plugins"] (S [ A"-I"; A"plugins"
      *                                        ; A"show.cmo";  A"gmap.cmo"
      *                                        ; A"foldl.cmo"; A"foldr.cmo"
      *                                        ; A"compare.cmo"; A"eq.cmo"
      *                                        ]); *)

     (* flag ["ocamldep"; "link_pa_gt"]   (S [ Sh"../camlp5o_pp.sh" ]);
      * flag ["compile";  "link_pa_gt"]   (S [ Sh"../camlp5o_pp.sh" ]); *)

    flag ["compile"; "native"; "use_gt"]   (S [ A"-I";A"src" ]);
    flag ["compile"; "byte";   "use_gt"]   (S [ A"-I";A"src" ]);
    flag ["link";    "byte";   "use_gt"]   (S [ A"-I";A"src"; A"GT.cma" ]);
    flag ["link";    "native"; "use_gt"]   (S [ A"-I";A"src"; A"GT.cmxa" ]);

    flag ["make_pp_gt"; "link"; "byte"] @@
    S ([ A"-package"; A"ppxlib"; A"common/GTCommon.cma" ] @
       make_plugins_args ~is_byte:true );
    flag ["make_pp_gt"; "link"; "native"] @@
    S ([ A"-package"; A"ppxlib"; A"common/GTCommon.cmxa"] @
       make_plugins_args ~is_byte:false);

     dep ["compile"; "use_ppx_extension"] ["ppx/ppx_deriving_gt.cma"; "rewriter/pp_gt.native"];
     ()
 | _ -> ()
)
