# our packages overlay
pkgs: _: with pkgs; {
  votingToolsHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
