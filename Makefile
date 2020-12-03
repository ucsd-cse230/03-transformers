.PHONY: all test clean ghcid distclean

all: test

test:
	stack test

clean:

ghcid:
	stack build ghcid && stack exec ghcid

distclean: clean
	stack clean

turnin: clean
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream https://github.com/ucsd-cse230/03-transformers.git

update:
	git pull upstream master
