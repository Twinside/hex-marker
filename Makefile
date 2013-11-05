
HOME:=$(HOMEPATH)
CABAL:=$(HOME)/AppData/Roaming/cabal/bin/cabal

build:
	$(CABAL) build

conf:
	$(CABAL) configure --enable-library-profiling

sandbox:
	$(CABAL) sandbox init

dep:
	$(CABAL) install --reinstall -p --only-dependencies

test:
	dist/build/marker-test/marker-test +RTS -xc -RTS mand_prgrsv.jpg o.html

