
dev:
	nix-shell --run "ghcid -c 'cabal repl $(target) --project-file=cabal-nix.project'"

repl:
	nix-shell --run "cabal repl $(target) --project-file=cabal-nix.project"

build:
	nix-build default.nix -A haskellPackages.voter-registration.components.exes.voter-registration -o voter-registration
