
dev:
	nix-shell --run "ghcid -c 'cabal repl $(target) --project-file=cabal-nix.project'"

repl:
	nix-shell --run "cabal repl $(target) --project-file=cabal-nix.project"

build-voting-tools:
	nix-build default.nix -A haskellPackages.voting-tools.components.exes.voting-tools -o voting-tools

style: ## Apply stylish-haskell on all *.hs files
	nix-shell --pure --run 'find . -type f -name "*.hs" -not -path ".git" -not -path "*.stack-work*" -print0 | xargs -0 stylish-haskell -i'

test-integration:
	nix-build -A integration-tests

test:
	nix-build default.nix -A haskellPackages.voting-tools.checks.unit-tests
