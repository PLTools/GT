# type in sources here
SOURCES = plugin.ml core.ml extension.ml 
SUBDIRS = doc

include $(top_srcdir)/m4/Makefile.am.vars

CMOPK = pa_gt.cmo
CLEAN_EXTRA += pa_gt.cmi
DEBUG = -g

# overwrite some variables
AM_P5FLAGS := -pp "$(CAMLP5O) $(PARSER_INCLUDES) pr_o.cmo"
PXFLAGS = $(P5FLAGS) 
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(top_srcdir)/src -I "$(CAMLP5LIB)"

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets
TARGET = $(CMOPK) pa_gt.cmi

.PHONY: doc
doc:

include $(top_srcdir)/m4/Makefile.am.rules
