# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? true
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:

with pkgs; with commonLib;
let

  sources = import ./nix/sources.nix {};

  cardano-node-nix =
    import (sources.cardano-node) { gitrev = sources.cardano-node.rev; };
  bech32 = cardano-node-nix.bech32;

  jormungandr-src = pkgs.fetchurl {
     url =
       "https://github.com/input-output-hk/jormungandr/releases/download/v0.9.3/jormungandr-0.9.3-x86_64-unknown-linux-musl-generic.tar.gz";
     sha256 = "sha256:14giz9yz94mdjrdr96rz5xsj21aacdw8mqrfdz031czh4qgnmnzh";
   };
   jormungandr =
     pkgs.runCommand "jormungandr" { buildInputs = [ pkgs.gnutar ]; } ''
       mkdir -p $out/bin
       cd $out/bin
       tar -zxvf ${jormungandr-src}
     '';

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = votingToolsHaskellPackages.shellFor {
    name = "voting-tools-shell";

    # If shellFor local packages selection is wrong,
    # then list all local packages then include source-repository-package that cabal complains about:
    packages = ps: with ps; [
       ps.voting-tools
       ps.voter-registration
    ];
    # packags = ps: pkgs.lib.attrValues (selectProjectPackages ps);

    tools = { cabal = "3.2.0.0"; };

    # These programs will be available inside the nix-shell.
    buildInputs = (with pkgs; [
      # cabal-install
      ghcid
      git
      hlint
      niv
      nix
      pkgconfig
      stylish-haskell
      jormungandr
      bech32
    ]);

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;

    GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  };

in

 shell
