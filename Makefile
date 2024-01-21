all:
	cabal build
	cp dist-newstyle/build/aarch64-osx/ghc-9.2.7/tonnetz-haskell-graph-0.1.0.0/x/tonnetz-haskell-graph/build/tonnetz-haskell-graph/tonnetz-haskell-graph .
	./tonnetz-haskell-graph > ../eubase/m3.txt
