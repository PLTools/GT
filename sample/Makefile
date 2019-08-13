BUILD=ocamlbuild -use-ocamlfind #-plugin-tag "package(ocaml-migrate-parsetree-ocamlbuild)"

all: tast
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) lambdas.native decoration.native

.PHONY: tast

tast:
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) tastrun.byte

tast_run:
	./tastrun.byte a.ml
	$(RM) -r *.cmi

clean:
	$(RM) -r _build *.native

