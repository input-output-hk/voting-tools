
dev:
	nix-shell --run "ghcid -c 'cabal repl $(target) --project-file=cabal-nix.project'"

repl:
	nix-shell --run "cabal repl $(target) --project-file=cabal-nix.project"

build-registration:
	nix-build default.nix -A haskellPackages.voter-registration.components.exes.voter-registration -o voter-registration

build-fetch:
	nix-build default.nix -A haskellPackages.fetch-registration.components.exes.fetch-registration -o fetch-registration

style: ## Apply stylish-haskell on all *.hs files
	nix-shell --pure --run 'find . -type f -name "*.hs" -not -path ".git" -not -path "*.stack-work*" -print0 | xargs -0 stylish-haskell -i'

test-integration:
	nix-build -A integration-tests
