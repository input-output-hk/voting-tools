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
      url = "github:input-output-hk/cardano-db-sync?ref=refs/tags/12.0.2";
    };
    tullia.url = "github:input-output-hk/tullia";
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix, cardano-db-sync, tullia, ... } @ inputs:
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

      mkHydraJobs = system: let
        jobs = recursiveUpdate self.packages.${system} self.checks.${system} // {
          nixosTests = import ./nix/nixos/tests/default.nix {
            inherit system inputs;
            pkgs = self.legacyPackages.${system};
          };
        };
      in
        jobs // {
          required = with nixpkgs.legacyPackages.${system}; releaseTools.aggregate {
            name = "github-required";
            meta.description = "All jobs required to pass CI";
            constituents = lib.collect lib.isDerivation jobs ++ lib.singleton
              (writeText "forceNewEval" self.rev or "dirty");
          };
        };
    in
      eachSystem supportedSystems (system:
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

            votingToolsTarball = pkgs.runCommandNoCC "voting-tools-tarball" { buildInputs = [ pkgs.gnutar pkgs.gzip ]; } ''
              cp ${flake.packages."x86_64-unknown-linux-musl:voting-tools:exe:voting-tools"}/bin/voting-tools ./
              mkdir -p $out/nix-support
              tar -czvf $out/voting-tools.tar.gz voting-tools
              echo "file binary-dist $out/voting-tools.tar.gz" > $out/nix-support/hydra-build-products
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
        } //
          tullia.fromSimple system (import nix/tullia.nix)
      ) // {
        # We can't use flake-utils' eachSystem for this
        # because, unexpectedly, it outputs a different structure for hydraJobs.
        hydraJobs = let
          perSystem = lib.genAttrs supportedSystems mkHydraJobs;
        in perSystem // {
          required = with nixpkgs.legacyPackages.${defaultSystem}; releaseTools.aggregate {
            name = "github-required";
            meta.description = "All jobs required to pass CI";
            constituents =
              map (s: s.required) (__attrValues perSystem) ++
              lib.singleton (writeText "forceNewEval" self.rev or "dirty");
          };
        };
      };
}
