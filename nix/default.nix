{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
, ownHaskellNix ? null
}:
let
  flakeSources = let
    flakeLock = (builtins.fromJSON (builtins.readFile ../flake.lock)).nodes;
    compat = s: builtins.fetchGit {
      url = "https://github.com/${s.locked.owner}/${s.locked.repo}.git";
      inherit (s.locked) rev;
      ref = s.original.ref or "master";
    };
  in {
    "haskell.nix" = compat flakeLock.haskellNix;
    "iohk-nix" = compat flakeLock.iohkNix;
    "cardano-node" = compat flakeLock.cardano-node;
  };
  sources = flakeSources // sourcesOverride;
  iohkNix = import sources.iohk-nix {};
  haskellNix = import sources."haskell.nix" { inherit system sourcesOverride; };
  nixpkgs = haskellNix.sources.nixpkgs-2105;

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNix.overlays.haskell-nix-extra
    # iohkNix: nix utilities and niv:
    ++ iohkNix.overlays.iohkNix
    # iohkNix: utils
    ++ iohkNix.overlays.utils
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {

        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // iohkNix // iohkNix.cardanoLib
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources and overlays
          // { inherit overlays sources nixpkgs; };

        svcLib = import ./svclib.nix { inherit pkgs; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.nixpkgsArgs.config // config;
  };

in pkgs
