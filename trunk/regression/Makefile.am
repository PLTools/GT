TESTS = test000 test001 test002 test003 test004 test008 test010 test011

TESTS_ENVIRONMENT = $(srcdir)/test.sh

include $(top_srcdir)/m4/Makefile.am.vars

PARSER_INCLUDES += -I $(top_srcdir)/camlp5
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMA += $(PACKAGE).cma

# OCaml parser call with flags
AM_P5FLAGS = -pp "$(CAMLP5O) $(PARSER_INCLUDES) pr_o.cmo ../camlp5/pa_gt.cmo -L ."

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(CAMLP5LIB)

TARGET = show.cmo 

CLEAN_EXTRA += $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS))
CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log show.cmo

%.cmi: %.mli
	$(OCAMLC) -c -pp $(CAMLP5O) -I ../src $<
show.cmo: show.ml
	$(OCAMLC) -c -pp $(CAMLP5O) -I +camlp5 -I ../src -I ../camlp5 GT.cma show.ml

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
