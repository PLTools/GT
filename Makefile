.PHONY: discover-tests test promote clean celan

all:
	dune build 

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

uninstall:
	dune build @install
	dune uninstall


