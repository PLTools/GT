# type in sources here
SOURCES = pa_got.ml
SUBDIRS = doc

include $(top_srcdir)/m4/Makefile.am.vars

# overwrite some variables
PXFLAGS = -pp "$(CAMLP4OF) $(PARSER_INCLUDES)"
CAMLPXLIB = $(CAMLP4LIB)
MODULE_INCLUDES += -I "$(CAMLP4LIB)"

include $(top_srcdir)/m4/Makefile.am.targets

# overwrite some targets
TARGET := $(CMO) $(CMI)

.PHONY: doc
doc:
	for dir in $(SUBDIRS); do \
		(cd $$dir; make doc) \
	done

include $(top_srcdir)/m4/Makefile.am.rules
