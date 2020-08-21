.PHONY: discover-tests clean celan

all:
	dune build 

discover-tests:
	dune build @discover-tests


celan:
clean:	
	$(RM) -r _build

rebuild: clean
	$(MAKE) all tests
