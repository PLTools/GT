#!/usr/bin/env sh
#export OCAMLPATH=`pwd`/_build/bundle
#camlp5o -I `ocamlfind query GT` pa_gt.cmo pr_o.cmo -I plugins show.cmo gmap.cmo foldl.cmo foldr.cmo compare.cmo eq.cmo $@


camlp5o camlp5/pp5gt.cma plugins/foldl.cmo plugins/gmap.cmo \
        plugins/show.cmo \
        $@
exit $?

# camlp5o -I `ocamlfind query base` \
#         -I `ocamlfind query base`/caml \
#         -I `ocamlfind query base`/shadow_stdlib \
#         -I `ocamlfind query sexplib0` \
#         -I `ocamlfind query result` \
#         -I `ocamlfind query compiler-libs.common` \
#         -I `ocamlfind query ocaml-compiler-libs`/shadow \
#         -I `ocamlfind query ocaml-migrate-parsetree` \
#         -I `ocamlfind query stdio` \
#         -I `ocamlfind query ppx_derivers` \
#         -I `ocamlfind query ppxlib` \
#         -I `ocamlfind query ppxlib`/ast \
#         -I `ocamlfind query ppxlib`/traverse_builtins \
#         -I `ocamlfind query ppxlib`/print_diff \
#         sexplib0.cma caml.cma shadow_stdlib.cma base.cma \
#         ocamlcommon.cma stdio.cma \
#         migrate_parsetree.cma ppx_derivers.cma \
#         ppxlib_ast.cma ppxlib_traverse_builtins.cma ppxlib_print_diff.cma ocaml_shadow.cma \
#         ppxlib.cma  camlp5/pa_gt.cmo pr_o.cmo $@

#camlp5o -I `ocamlfind query base` -I `ocamlfind query base`/shadow_stdlib -I `ocamlfind query ppxlib` -I `ocamlfind query ppxlib`/ast -I `ocamlfind query base`/caml -I `ocamlfind query sexplib0` -I `ocamlfind query stdio`         sexplib0.cma caml.cma shadow_stdlib.cma base.cma stdio.cma ppxlib_ast.cma ppxlib.cma camlp5/pa_gt.cmo
