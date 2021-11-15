STACK       = stack --allow-different-user

.PHONY: all test clean ghcid distclean

all: test

test:   clean
	$(STACK) test

clean:

distclean: clean
	$(STACK) clean

ghcid:
	$(STACK) build ghcid && $(STACK) exec ghcid

turnin: clean
	git commit -a -m "turnin"
	git push origin master

upstream:
	git remote add upstream https://github.com/ucsd-cse230/03-transformers.git

update:
	git pull upstream master
