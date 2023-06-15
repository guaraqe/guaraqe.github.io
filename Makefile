.DEFAULT_GOAL := help

################################################################################
# Website

build: ## Build the website.
	cabal run --ghc-options="-O0" -- builder

.PHONY: build

serve: ## Serve the website with live reload.
	livereload docs

.PHONY: serve

################################################################################
# Haskell development

ghcid: ## Launch ghcid for dars-server.
	ghcid --command 'cabal repl builder'

.PHONY: ghcid

format-haskell: ## Format the Haskell code
	ormolu --mode inplace $$(git ls-files '*.hs')
	cabal-fmt --inplace $$(git ls-files '*.cabal')

.PHONY: format-haskell

################################################################################
# Help

help: ## Print the list of commands.
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

.PHONY: help
