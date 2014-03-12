SUBDIRS = camlp5 src regression 

.PHONY: doc
doc:
	for dir in $(SUBDIRS); do \
		test "$$dir" = "." || (cd $$dir; make doc) \
	done
