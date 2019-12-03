#!/usr/bin/env sh

set +x
# TODO: camlp5 doesn't show backtraces
export OCAMLRUNPARAM=b
camlp5o `ocamlfind query ocamlgraph`/graph.cma \
	camlp5/pp5gt.cma \
	plugins/gmap.cmo plugins/show.cmo \
	plugins/compare.cmo plugins/eq.cmo \
	plugins/foldl.cmo plugins/foldr.cmo \
	plugins/stateful.cmo plugins/eval.cmo \
	plugins/html.cmo \
	plugins/gfmt.cmo \
	plugins/hash.cmo \
	$@
exit $?

