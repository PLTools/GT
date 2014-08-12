TESTS = test000 test001 test002 test003 test004 test005 test006 test007\
test008 test009 test010 test011 test012 test013 test014 test015 test016\
test017 test018 test019 test020 test021 test022 test023 test024 test025\
test026 test027 test028 test029 test030 test031 test032 

# Problematic tests: 
#   test033 --- mutual recursion and extensibility
#   test025 --- does not work with pr_o.cmo (camlp5 parser is wider than ocaml's original; 
#               the reverted text is not parsed while the tree is ok)
#   test034 with no pr_o.cmo causes ocamlc to stack overflow (bug in the compiler?)


TESTS_ENVIRONMENT = $(srcdir)/test.sh

include $(top_srcdir)/m4/Makefile.am.vars

PARSER_INCLUDES += -I $(top_srcdir)/camlp5
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMA += $(PACKAGE).cma

# OCaml parser call with flags
AM_P5FLAGS = -pp "$(CAMLP5O) $(PARSER_INCLUDES) ../camlp5/pa_gt.cmo -L ../plugins"

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(CAMLP5LIB)

TARGET = 

CLEAN_EXTRA += $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS))
CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log

%.cmi: %.mli
	$(OCAMLC) -c -pp $(CAMLP5O) -I ../src $<

include $(top_srcdir)/m4/Makefile.am.targets

# do not install anyting
install-data-am:
	@true
uninstall-am:
	@true
# do not call ocamldep
.depend:
	touch .depend

# Rules to build tests
%: %.ml %.cmi
	$(OCAMLC) $(BFLAGS) $(EXTRA_CMA) $(PXFLAGS) -o $@$(EXEEXT) $<

include $(top_srcdir)/m4/Makefile.am.rules
