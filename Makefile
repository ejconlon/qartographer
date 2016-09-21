.PHONY: all

all: format lint

lint:
	stack exec hlint -- src/Qartographer/Core/*.hs src/Qartographer/Server/*.hs app/*.hs test/*.hs

format:
	stack exec stylish-haskell -- -i src/Qartographer/Core/*.hs src/Qartographer/Server/*.hs app/*.hs test/*.hs

deps:
	stack install hlint stylish-haskell
