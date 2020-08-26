.PHONY: discover-tests test promote clean celan

# compiler packages without tests
all:
	dune build -p GT,GT-p5

discover-tests:
	dune build @discover-tests

test:
	dune runtest

promote:
	dune promote

celan: clean
clean:
	$(RM) -r _build

rebuild: clean
	$(MAKE) all tests

install:
	dune build @install
	dune install
	# echo "(lang dune 2.7)" > `ocamlfind query GT`/dune-package
	# echo "(use_meta)" >> `ocamlfind query GT`/dune-package
	# dune describe --only-packages=GT >> `ocamlfind query GT`/dune-package


uninstall:
	dune build @install
	dune uninstall
