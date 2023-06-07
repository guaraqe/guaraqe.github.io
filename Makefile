.DEFAULT_GOAL := help

################################################################################
# Server

build: ## Build the website.
	cd builder && cabal run --ghc-options="-O0" -- builder

.PHONY: build

################################################################################
# Haskell development

ghcid: ## Launch ghcid for dars-server.
	cd builder && ghcid --command 'cabal repl'

.PHONY: ghcid

format-haskell: ## Format the Haskell code
	cd builder && ormolu --mode inplace $$(git ls-files '*.hs')
	cd builder && cabal-fmt --inplace $$(git ls-files '*.cabal')

.PHONY: format-haskell

################################################################################
# Help

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
