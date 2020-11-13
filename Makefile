.PHONY: discover-tests test promote clean celan

# compiler packages without tests
all:
	dune build -p GT,GT-p5

discover-tests:
	echo "" > regression/dune.tests
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
