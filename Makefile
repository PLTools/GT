PKGNAME=GT
MKDIR ?= mkdir -vp
CP    ?= cp

OB=ocamlbuild -use-ocamlfind -classic-display #-plugin-tag "package(ocaml-migrate-parsetree-ocamlbuild)" #-classic-display -ignore Test025
ifdef OBV
OB += -verbose 6
endif

CMA_TARGETS=src/GT.cma
CMO_TARGETS=
BYTE_TARGETS=$(CMA_TARGETS) $(CMO_TARGETS)
NATIVE_TARGETS= $(CMA_TARGETS:.cma=.cmxa) $(CMO_TARGETS:.cmo=.cmx)
TESTS_ENVIRONMENT=./test.sh

.DEFAULT_GOAL :=  all

.PHONY: all syntax lib camlp5 all_plugins ppx bundle samples
.PHONY: add_common common add_lib lib add_camlp5 camlp5
.PHONY: celan clean rebuild clean_tests install uninstall
.PHONY: tests test regression promote

.DEFAULT_GOAL: all

OBTARGETS=
OBPARAMS=
all: add_common add_plugins add_camlp5 add_lib add_ppx compile \
		bundle #standalone_rewriter bundle

compile:
	$(OB) $(OBPARAMS) $(OBTARGETS)

add_common:
	$(eval OBTARGETS +=  common/GTCommon.cma common/GTCommon.cmxa)
	#$(eval OBPARAMS  += -I plugins)
common: add_common compile
add_lib:
	$(eval OBTARGETS += src/GT.cma src/GT.cmxa )
lib: add_lib compile
add_camlp5: add_common
	$(eval OBTARGETS += camlp5/pa_gt.cma camlp5/pp5gt.cma)
	#$(eval OBPARAMS  += -I common)
camlp5: add_camlp5 compile

ppx: add_common add_plugins add_ppx compile
add_ppx:
	$(eval OBTARGETS += ppx/ppx_deriving_gt.cma ppx/ppx_deriving_gt.cmxs ppx/pp_gt.native)
	#$(eval OBPARAMS  += -I common -I plugins)

PLUGINS=compare eq foldl foldr gmap show show_typed html
add_plugins:
	$(eval OBPARAMS  += -I common)
	$(eval OBTARGETS += $(addprefix plugins/,$(addsuffix .cmo,$(PLUGINS))) \
											$(addprefix plugins/,$(addsuffix .cmx,$(PLUGINS))) )
plugins: add_plugins compile

celan: clean

clean: clean_tests
	$(RM) -r _build *.log  *.native *.byte


######################## Tests related stuff  ##########################
REGRES_CASES := #807 029 037 811 900 809 808 801 802 803 804 806 #807 #805
# now we add camlp5 tests
REGRES_CASES += 000 037 081 082 083 086 089 029

TEST_DIR := regression
define TESTRULES
ML_FILE_$(1) = $(wildcard regression/test$(1)*.ml)
#$$(info $$(ML_FILE_$(1)) )
NATIVE_$(1) := $$(patsubst %.ml,%.native,$$(ML_FILE_$(1)) )
#BYTE_TEST_EXECUTABLES += BYTE_$(1)
NATIVE_TEST_EXECUTABLES += $$(NATIVE_$(1))
#$$(info $$(NATIVE_TEST_EXECUTABLES) )

TEST$(1)_NAME := $$(ML_FILE_$(1):regression/test$(1)%.ml=%)
#$$(info $$(ML_FILE_$(1)) $$(NATIVE_$(1)) $$(TEST$(1)_NAME) )
#$$(info $$(NATIVE_TEST_EXECUTABLES)) 
.PHONY: test_$(1) test$(1).native compile_tests_native compile_tests_byte

test$(1).native: $$(ML_FILE_$(1)) all $$(NATIVE_$(1))
test$(1).byte:   all

$$(NATIVE_$(1)):
	OCAMLPATH=`pwd`/_build/bundle \
	$(OB) -classic-display $$@

run_tests: test_$(1)
test_$(1):
	@cd $$(TEST_DIR) && $(TESTS_ENVIRONMENT) ../$$(notdir $$(NATIVE_$(1))); \
	if [ $$$$? -ne 0 ] ; then echo "$(1) FAILED"; else echo "$(1) PASSED"; fi

promote_all: promote_$(1)
promote_test$(1): promote_$(1)
promote_$(1):
	./$$(notdir $$(NATIVE_$(1)) ) > regression/orig/test$(1)$$(TEST$(1)_NAME).log

endef
$(foreach i,$(REGRES_CASES),$(eval $(call TESTRULES,$(i)) ) )

.PHONY: compile_tests_native compile_tests_byte compile_tests run_tests

compile_tests_native:
	@echo "Adding " $(NATIVE_TEST_EXECUTABLES)
	$(eval OBTARGETS += $(NATIVE_TEST_EXECUTABLES))
	#$(eval OBPARAMS  += -I src)

compile_tests_byte:
	$(eval OBTARGETS += $(BYTE_TEST_EXECUTABLES))
	#$(eval OBPARAMS  += -I src)

compile_tests: compile_tests_native

clean_tests:
	$(RM) -r _build/regression

promote: promote_all

tests: #add_common add_plugins add_camlp5 add_lib compile
	$(MAKE) compile_tests compile run_tests

regression: tests
test: tests

######################## Installation related stuff ##########################
INSTALL_TARGETS=META \
	$(wildcard _build/common/GTCommon.cma) \
	$(wildcard _build/common/GTCommon.cmxa) \
	$(wildcard _build/common/expander.cmi) \
	_build/camlp5/pa_gt.cma \
	_build/camlp5/pp5gt.cma \
	$(wildcard _build/src/GT.cmx)  \
	$(wildcard _build/src/GT.cma)  \
	$(wildcard _build/src/GT.cmxa) \
	$(wildcard _build/src/GT.a)   \
	$(wildcard _build/src/View.cmi) \
	$(wildcard _build/src/HTML.cmi) \
	$(wildcard _build/src/GT.cmi) \
	$(wildcard _build/ppx/ppx_deriving_gt.a) \
	$(wildcard _build/ppx/ppx_deriving_gt.cma) \
	$(wildcard _build/ppx/ppx_deriving_gt.cmxa) \
	$(wildcard _build/ppx/ppx_deriving_gt.cmxs) \
	$(wildcard _build/ppx/ppx_deriving_gt.cmi) \
	$(wildcard _build/ppx/ppx_gt_expander.cma) \
	$(wildcard _build/ppx/ppx_gt_expander.cmxa) \
	$(wildcard _build/ppx/ppx_gt_expander.cmi) \
	$(wildcard _build/ppx/ppx_gt_expander.a) \
	$(wildcard _build/rewriter/pp_gt.native) \
	$(wildcard $(addprefix _build/plugins/,$(addsuffix .cmo,$(PLUGINS))) ) \
	$(wildcard $(addprefix _build/plugins/,$(addsuffix .cmx,$(PLUGINS))) ) \
	$(wildcard $(addprefix _build/plugins/,$(addsuffix .o,$(PLUGINS))) ) \

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

rebuild: clean
	$(MAKE) all tests

