.PHONY: install clean

PREFIX ?= /usr/local/bin

build/punch: *.hs
	mkdir -p build
	ghc -Wall -Werror -odir build -hidir build -o build/punch --make ${GHC_FLAGS} Main

install: clean
	GHC_FLAGS="-Odph" $(MAKE) build/punch
	cp build/punch ${PREFIX}

clean:
	rm -r build
