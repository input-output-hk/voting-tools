{
  description = "Cardano Node";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, haskell-nix, ... }:
    (utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        legacyPackages = import ./nix {
          inherit system;
          ownHaskellNix = haskell-nix.legacyPackages.${system};
        };

        lib = nixpkgs.lib;

        sources = import ./nix/sources.nix { };
        iohkNix = import sources.iohk-nix { inherit system; };

        names = [ "voter-registration" "voting-tools" ];
        eachName = f:
          lib.listToAttrs
          (lib.forEach names (name: lib.nameValuePair name (f name)));

        packages = eachName (name:
          legacyPackages.votingToolsHaskellPackages.${name}.components.exes.${name});
      in {
        inherit legacyPackages packages;

        apps = eachName (name: utils.lib.mkApp {
          drv = packages.${name};
          exePath = "/bin/${name}";
        });
      }));
}
