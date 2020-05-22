.PHONY: all test clean ghcid distclean

all: test

test:
	stack test

clean:

ghcid:
	stack build ghcid && stack exec ghcid

distclean: clean
	stack clean
