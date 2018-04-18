PKGNAME=GT
MKDIR ?= mkdir -vp
CP    ?= cp

OB=ocamlbuild -use-ocamlfind -plugin-tag "package(ocaml-migrate-parsetree-ocamlbuild)" #-classic-display -ignore Test025
ifdef OBV
OB += -verbose 6
endif

CMA_TARGETS=src/GT.cma
CMO_TARGETS=
BYTE_TARGETS=$(CMA_TARGETS) $(CMO_TARGETS)
NATIVE_TARGETS= $(CMA_TARGETS:.cma=.cmxa) $(CMO_TARGETS:.cmo=.cmx)
TESTS_ENVIRONMENT=./test.sh

.DEFAULT_GOAL :=  all

.PHONY: all syntax lib plugins ppx bundle samples
.PHONY: celan clean clean_tests install uninstall
.PHONY: tests test regression promote

.DEFAULT_GOAL: all

all: syntax lib plugins ppx bundle

lib:
	$(OB) -Is src $(BYTE_TARGETS) $(NATIVE_TARGETS)

syntax:
	$(OB) camlp5/pa_gt.cmo

ppx:
	$(OB) -Is src ppx/ppx_deriving_gt.cma ppx/ppx_deriving_gt.cmxs \
		ppx/ppx_gt_expander.cma ppx/ppx_gt_expander.cmxa \
		rewriter/pp_gt.native

PLUGINS=compare eq foldl foldr html gmap show typename
plugins: syntax
	$(OB) $(addprefix plugins/,$(addsuffix .cmo,$(PLUGINS)))

celan: clean

clean: clean_tests
	$(RM) -r _build *.log  *.native *.byte


######################## Tests related stuff  ##########################
REGRES_CASES := #$(shell seq -s \  -w 1 024) $(shell seq -s \  -w 26 028)
REGRES_CASES := #795intoption #799 798 798gen 796scheme_gen 794camlp5 790showF 791showT #705
REGRES_CASES := 808 801 802 803 804 806 #807 #805
#$(warning $(REGRES_CASES) )

define TESTRULES
ML_FILE_$(1) = $(wildcard regression_ppx/test$(1)*.ml)

NATIVE_$(1) := $$(patsubst %.ml,%.native,$$(ML_FILE_$(1)) )
#BYTE_TEST_EXECUTABLES += BYTE_$(1)
NATIVE_TEST_EXECUTABLES += $$(NATIVE_$(1))

TEST$(1)_NAME := $$(ML_FILE_$(1):regression_ppx/test$(1)%.ml=%)
#$$(info $$(ML_FILE_$(1)) $$(NATIVE_$(1)) $$(TEST$(1)_NAME) )
#$$(info $$(NATIVE_TEST_EXECUTABLES)) 
.PHONY: test_$(1) test$(1).native compile_tests_native compile_tests_byte

test$(1).native: all $$(NATIVE_$(1))
test$(1).byte:   all

$$(NATIVE_$(1)):
	OCAMLPATH=`pwd`/bundle \
	$(OB) -classic-display $$@

run_tests: test_$(1)
test_$(1):
	@cd regression_ppx && $(TESTS_ENVIRONMENT) ../$$(notdir $$(NATIVE_$(1))); \
	if [ $$$$? -ne 0 ] ; then echo "$(1) FAILED"; else echo "$(1) PASSED"; fi

promote_all: promote_$(1)
promote_test$(1): promote_$(1)
promote_$(1):
	./$$(notdir $$(NATIVE_$(1)) ) > regression_ppx/orig/test$(1)$$(TEST$(1)_NAME).log

endef
$(foreach i,$(REGRES_CASES),$(eval $(call TESTRULES,$(i)) ) )

.PHONY: compile_tests_native compile_tests_byte compile_tests run_tests

compile_tests_native: all $(TEST_MLS)
	OCAMLPATH=`pwd`/_build/bundle \
	$(OB) $(NATIVE_TEST_EXECUTABLES)

compile_tests_byte: all $(TEST_MLS)
	$(OB) -plugin-tag "package(ppx_driver.ocamlbuild)" $(BYTE_TEST_EXECUTABLES)

compile_tests: compile_tests_native

clean_tests:
	$(RM) -r _build/regression

promote: promote_all

tests: all compile_tests run_tests
regression: tests
test: tests

######################## Installation related stuff ##########################
INSTALL_TARGETS=META \
	$(wildcard _build/camlp5/pa_gt.cmo) \
	_build/src/GT.cmx \
	_build/src/GT.cma \
	_build/src/GT.cmxa \
	_build/src/GT.a \
	_build/src/View.cmi \
	_build/src/HTML.cmi \
	_build/src/GT.cmi \
	_build/ppx/ppx_deriving_gt.a \
	_build/ppx/ppx_deriving_gt.cma \
	_build/ppx/ppx_deriving_gt.cmxa \
	_build/ppx/ppx_deriving_gt.cmxs \
	_build/ppx/ppx_deriving_gt.cmi \
	_build/rewriter/pp_gt.native \
	_build/ppx/ppx_gt_expander.cma \
	_build/ppx/ppx_gt_expander.cmxa \
	_build/ppx/ppx_gt_expander.cmi \
	_build/ppx/ppx_gt_expander.a \
	$(wildcard $(addprefix _build/plugins/,$(addsuffix .cmo,$(PLUGINS))) ) \

BUNDLEDIR=_build/bundle/$(PKGNAME)

define MAKE_BUNDLE_RULE
$(BUNDLEDIR)/$(notdir $(1)): $(1)
	@cp $(1) $(BUNDLEDIR)
MAKE_BUNDLE_TARGETS += $(BUNDLEDIR)/$(notdir $(1))

endef
$(foreach i,$(INSTALL_TARGETS),$(eval $(call MAKE_BUNDLE_RULE,$(i)) ) )

rmbundledir:
	@$(RM) -r $(BUNDLEDIR)

$(BUNDLEDIR):
	@$(MKDIR) $@

bundle: rmbundledir $(BUNDLEDIR) $(MAKE_BUNDLE_TARGETS)

samples: bundle
	$(MAKE) -C sample

install: bundle
	ocamlfind install $(PKGNAME) $(BUNDLEDIR)/*

uninstall:
	ocamlfind remove $(PKGNAME)
