
dev:
	nix-shell --run "ghcid -c 'cabal repl $(target) --project-file=cabal-nix.project'"

repl:
	nix-shell --run "cabal repl $(target) --project-file=cabal-nix.project"

build-voting-tools:
	nix-build default.nix -A haskellPackages.voting-tools.components.exes.voting-tools -o voting-tools

build-voter-registration:
	nix-build default.nix -A haskellPackages.voter-registration.components.exes.voter-registration -o voter-registration

style: ## Apply stylish-haskell on all *.hs files
	nix-shell --pure --run 'find . -type f -name "*.hs" -not -path ".git" -not -path "*.stack-work*" -not -path "./dist*" -print0 | xargs -0 stylish-haskell -i'

test:
	nix-build default.nix -A haskellPackages.voting-tools.checks.unit-tests
