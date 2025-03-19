.PHONY: release discover-tests test promote clean celan doc

# compiler packages without tests
all:
	dune build $(DFLAGS)

release: DFLAGS += --profile=release
release: all

discover-tests:
	echo "" > regression/dune.tests
	$(RM) -fr _build/default/config
	dune build @discover-tests
	cp _build/default/config/*.t regression/

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
	$(RM) -r _build _coverage

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

.PHONY: odig
ODIG_SWITCHES = --odoc-theme=odig.gruvbox.light
ODIG_SWITCHES += --no-tag-index
ODIG_SWITCHES += --no-pkg-deps
odig:
	dune install $(DFLAGS) GT
	odig odoc $(ODIG_SWITCHES) GT

.PHONY: coverage
TEST_COV_D ?= /tmp/GTcov
coverage:
	if [ -d $(TEST_COV_D) ]; then $(RM) -r $(TEST_COV_D); fi
	mkdir -p $(TEST_COV_D)
	BISECT_FILE=$(TEST_COV_D)/GT dune runtest --no-print-directory \
		--instrument-with bisect_ppx --force
	bisect-ppx-report html --coverage-path $(TEST_COV_D) #--expect src/
	bisect-ppx-report summary --coverage-path $(TEST_COV_D) #--expect src/
