BUILD=ocamlbuild -use-ocamlfind #-plugin-tag "package(ocaml-migrate-parsetree-ocamlbuild)"

all: camlast
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) lambdas.native decoration.native

.PHONY: camlast

camlast:
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) camlastrun.native

tast:
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) tastrun.native

clean:
	$(RM) -r _build *.native

