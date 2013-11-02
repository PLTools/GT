ocamlc -c -I +camlp5 -pp "camlp5o" show.ml 
ocamlopt -o sample -pp "camlp5o pa_gt.cmo -L ." -I `ocamlfind -query GT` GT.cmxa sample.ml
ocamlopt -o murec -pp "camlp5o pa_gt.cmo -L ." -I `ocamlfind -query GT` GT.cmxa murec.ml
