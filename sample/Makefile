BUILD=ocamlbuild -use-ocamlfind #-plugin-tag "package(ocaml-migrate-parsetree-ocamlbuild)"

all: tast
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) lambdas.native decoration.native

.PHONY: tast

tast:
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) tastrun.native

tast_run:
	./tastrun.native a.ml
	$(RM) -r *.cmi

clean:
	$(RM) -r _build *.native

