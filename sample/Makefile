BUILD=ocamlbuild -use-ocamlfind -plugin-tag "package(ocaml-migrate-parsetree-ocamlbuild)"

all:
	OCAMLPATH=`pwd`/../_build/bundle $(BUILD) lambdas.native

clean:
	$(RM) -r _build *.native

