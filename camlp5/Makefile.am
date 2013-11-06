# type in sources here
SOURCES = plugin.ml core.ml extension.ml
MLI = 
SUBDIRS = doc

include $(top_srcdir)/m4/Makefile.am.vars

CMOPK = pa_gt.cmo
CLEAN_EXTRA += pa_gt.cmi

# overwrite some variables
PXFLAGS = -pp "$(CAMLP5O) $(PARSER_INCLUDES)"
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(top_srcdir)/src -I "$(CAMLP5LIB)"

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets
TARGET := $(CMOPK)

.PHONY: doc
doc:

include $(top_srcdir)/m4/Makefile.am.rules
