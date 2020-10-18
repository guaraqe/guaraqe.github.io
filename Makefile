build:
	nix-shell --command "cabal build"

.PHONY: build

generate:
	nix-shell --command "cabal exec -- www build"

.PHONY: generate

sync:
	rsync -ave ssh --delete /home/juan/aii/www/build/ a4555@guaraqe.gq:/home/a4555/public_html/

.PHONY: sync

nix:
	cabal2nix . --shell > shell.nix

.PHONY: nix
