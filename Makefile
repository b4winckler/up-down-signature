all:
	cabal-dev install --ghc-options="-Wall -O2"

profiling:
	cabal-dev install-deps --enable-library-profiling
	cabal-dev install --ghc-options="-fforce-recomp -prof -fprof-auto -auto-all -rtsopts"

sampling:
	# Profile paths with sampling
	./cabal-dev/bin/sigscore ./data/m100n10p10.txt +RTS -p -s -hc
	hp2ps -c sigscore.hp

exact:
	# Profile paths without sampling (nb. 3^10 = 59049 < 100000 which
	# causes exact score to be computed)
	./cabal-dev/bin/sigscore ./data/m3n10p10.txt --iter 100000 +RTS -p -s -hc
	hp2ps -c sigscore.hp

clean:
	rm -rf *.aux *.prof *.hp *.ps

distclean:
	rm -rf *.aux *.prof *.hp *.ps ./cabal-dev ./dist
