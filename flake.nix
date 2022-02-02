{
  description = "Voting Tools";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2105";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    cardano-node = {
      url = "github:input-output-hk/cardano-node?ref=refs/tags/1.31.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.haskellNix.follows = "haskellNix";
    };
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix, ... }:
    let
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault
        getAttrs optionalAttrs nameValuePair attrNames;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      supportedSystems = import ./supported-systems.nix;
      defaultSystem = head supportedSystems;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.crypto
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          gitrev = self.rev or "dirty";
          commonLib = lib // iohkNix.lib;
        })
        (import ./nix/pkgs.nix)
      ];

    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        flake = pkgs.votingToolsHaskellPackages.flake {
          crossPlatforms = p: with p; [
            mingwW64
            musl64
          ];
        };
        packages = collectExes
           flake.packages;

      in recursiveUpdate flake {

        inherit packages;

        legacyPackages = pkgs;

        # Built by `nix build .`
        defaultPackage = flake.packages."voting-tools:exe:voting-tools";

        # Run by `nix run .`
        defaultApp = flake.apps."voting-tools:exe:voting-tools";

        inherit (flake) apps;
      }
    );
}
