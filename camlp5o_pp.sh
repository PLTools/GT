#!/usr/bin/env sh
export OCAMLPATH=`pwd`/_build/bundle 
camlp5o -I `ocamlfind query GT` pa_gt.cmo pr_o.cmo -I plugins show.cmo gmap.cmo foldl.cmo foldr.cmo compare.cmo eq.cmo $@
