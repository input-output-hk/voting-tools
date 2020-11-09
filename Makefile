
dev:
	nix-shell --run "ghcid -c 'cabal repl $(target) --project-file=cabal-nix.project'"
