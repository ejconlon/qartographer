.PHONY: all

all: format lint

sources = src/Qartographer/Client/*.hs src/Qartographer/Core/*.hs src/Qartographer/Integration/*.hs src/Qartographer/Server/*.hs app/*.hs test/*.hs

lint:
	stack exec hlint -- $(sources)

format:
	stack exec stylish-haskell -- -i $(sources)

deps:
	stack install hlint stylish-haskell
