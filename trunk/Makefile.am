SUBDIRS = src regression camlp5

.PHONY: doc
doc:
	for dir in $(SUBDIRS); do \
		test "$$dir" = "." || (cd $$dir; make doc) \
	done
