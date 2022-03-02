{
  description = "Voting Tools";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-2111";
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
    };
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
    };
    cardano-node = {
      url = "github:input-output-hk/cardano-node?ref=refs/tags/1.31.0";
    };
    cardano-db-sync = {
      url = "github:input-output-hk/cardano-db-sync";
    };
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix, cardano-db-sync, ... } @ inputs:
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

    in (eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        flake = pkgs.votingToolsHaskellProject.flake {
          crossPlatforms = p: with p; [
            musl64
          ];
        };
        packages = collectExes flake.packages // {
          voterRegistrationTarball = pkgs.runCommandNoCC "voter-registration-tarball" { buildInputs = [ pkgs.gnutar pkgs.gzip ]; } ''
            cp ${flake.packages."x86_64-unknown-linux-musl:voter-registration:exe:voter-registration"}/bin/voter-registration ./
            mkdir -p $out/nix-support
            tar -czvf $out/voter-registration.tar.gz voter-registration
            echo "file binary-dist $out/voter-registration.tar.gz" > $out/nix-support/hydra-build-products
          '';
        };

      in recursiveUpdate flake {

        inherit packages;

        legacyPackages = pkgs;

        devShells.stylish = pkgs.mkShell { packages = with pkgs; [ stylish-haskell git ]; };

        # Built by `nix build .`
        defaultPackage = flake.packages."voting-tools:exe:voting-tools";

        # Run by `nix run .`
        defaultApp = flake.apps."voting-tools:exe:voting-tools";

        inherit (flake) apps;
      }
    )) // {
      hydraJobs = let jobs = lib.recursiveUpdate self.packages self.checks; in
        jobs // {
          required = with self.legacyPackages.${lib.head supportedSystems}; releaseTools.aggregate {
            name = "github-required";
            meta.description = "All jobs required to pass CI";
            constituents = lib.collect lib.isDerivation jobs ++ lib.singleton
              (writeText "forceNewEval" self.rev or "dirty");
          };
        };
    };
}
