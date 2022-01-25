.PHONY: release discover-tests test promote clean celan doc

# compiler packages without tests
all:
	dune build $(DFLAGS)

release: DFLAGS += --profile=release
release: all

discover-tests:
	echo "" > regression/dune.tests
	dune build @discover-tests

doc:
	dune build @doc $(DFLAGS)

doc-sphinx:
	echo TODO

test:
	dune runtest $(DFLAGS)

promote:
	dune promote $(DFLAGS)

celan: clean
clean:
	$(RM) -r _build

rebuild: clean
	$(MAKE) all tests

watch:
	$(MAKE) all DFLAGS=-w

install:
	dune build @install $(DFLAGS)
	dune install $(DFLAGS)

uninstall:
	dune build @install $(DFLAGS)
	dune uninstall $(DFLAGS)
