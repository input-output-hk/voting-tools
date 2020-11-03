# our packages overlay
pkgs: _: with pkgs; {
  voterRegistrationHaskellPackages = import ./haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
