{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-shell '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages votingToolsHaskellPackages);
  haskellPackagesMusl64 = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages pkgs.pkgsCross.musl64.votingToolsHaskellPackages);
  votingToolsTarball = pkgs.runCommandNoCC "voting-tools-tarball" { buildInputs = [ pkgs.gnutar gzip ]; } ''
    cp ${haskellPackagesMusl64.voting-tools.components.exes.voter-registration}/bin/voter-registration ./
    cp ${haskellPackagesMusl64.voting-tools.components.exes.fetch-registration}/bin/fetch-registration ./
    mkdir -p $out
    tar -czvf $out/voter-registration.tar.gz voter-registration
    tar -czvf $out/fetch-registration.tar.gz fetch-registration
    tar -czvf $out/voting-tools.tar.gz voter-registration fetch-registration
  '';

  self = {
    inherit votingToolsHaskellPackages votingToolsTarball;
    inherit haskellPackages hydraEvalErrors;

    inherit (pkgs.iohkNix) checkCabalProject;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
  };
in self
