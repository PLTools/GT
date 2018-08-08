open Base
open Stdio
module C = Configurator

let write_sexp fn sexp =
  Out_channel.write_all fn ~data:(Sexp.to_string sexp)

let ask_ocamlfind pkgname =
  let outfile = "/tmp/cfgout" in
  let _:int = Caml.Sys.command @@
    Printf.sprintf "ocamlfind query %s > %s" pkgname outfile in
  In_channel.read_lines outfile |> List.hd_exn

let () =
  C.main ~name:"mylib" (fun c ->
    let default : C.Pkg_config.package_conf =
      { libs   = ["-lblah"]
      ; cflags = []
      }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
        Option.value (C.Pkg_config.query pc ~package:"Qt5Quick") ~default
    in

    write_sexp "c_flags.sexp"         (sexp_of_list sexp_of_string conf.cflags);
    write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs)
    );
  (* let camlp5 = ask_ocamlfind "camlp5" in
   * let ccc = ask_ocamlfind "compiler-libs.common" in *)
  let og = ask_ocamlfind "ocamlgraph" in
  let og = ask_ocamlfind "ocamlgraph" in
  let data =
    Printf.sprintf
      "%s/graph.cma\n\
       camlp5/pp5gt.cma\n\
       common/GTCommon.cma\n\
       plugins/foldl.cma\n\
       plugins/foldr.cmo\n\
       plugins/gmap.cmo plugins/show.cmo\n\
       plugins/fmt.cmo\n\
       plugins/compare.cmo\n\
       plugins/eq.cmo\n\
       plugins/html.cmo\n\
       plugins/eval.cmo\n\
       plugins/stateful.cmo\n\
       plugins/show_typed.cmo"
      og
  in
  Out_channel.write_all "camlp5.cmd" ~data;
  ()
