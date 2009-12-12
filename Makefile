.PHONY: tests

all:
	@ghc --make Main

clean:
	@rm -f Main `find . -name '*.hi' -o -name '*.o'`

tests: all
	@python test.py
