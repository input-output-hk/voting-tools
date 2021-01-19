# our packages overlay
pkgs: _: with pkgs; {
  voterRegistrationHaskellPackages = import ./voter-registration-haskell.nix {
    inherit config
      lib
      stdenv
      haskell-nix
      buildPackages
      ;
  };
}
