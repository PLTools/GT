.PHONY: release discover-tests test promote clean celan doc
DFLAGS=

# compiler packages without tests
all:
	dune build -p GT,GT-p5 $(DFLAGS)

release: DFLAGS := --profile=release
release: all

discover-tests:
	echo "" > regression/dune.tests
	dune build @discover-tests

doc:
	dune build @doc

doc-sphinx:
	echo TODO

test:
	dune runtest $(DUNE_FLAGS)

promote:
	dune promote $(DUNE_FLAGS)

celan: clean
clean:
	$(RM) -r _build

rebuild: clean
	$(MAKE) all tests

install:
	dune build @install $(DUNE_FLAGS)
	dune install $(DUNE_FLAGS)

uninstall:
	dune build @install $(DUNE_FLAGS)
	dune uninstall $(DUNE_FLAGS)
