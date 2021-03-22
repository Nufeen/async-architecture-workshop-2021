c2nix:
	nix-shell --pure -p cabal2nix --run "cabal2nix ." > default.nix

init:
	nix-shell --pure -p ghc cabal-install --run "cabal init"
	nix-shell --pure shell.nix --run "cabal configure --enable-tests"

shell:
	nix-shell --pure shell.nix

build:
	make c2nix
	nix-build release.nix

repl:
	make c2nix
	nix-shell --pure shell.nix --run "cabal repl"

test:
	nix-shell --pure shell.nix --run "cabal test"

ghcid:
	make c2nix
	nix-shell --pure shell.nix --run "ghcid"

.PHONY:
	c2nix
