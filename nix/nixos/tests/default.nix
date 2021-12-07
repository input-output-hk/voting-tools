{ pkgs
, system
, supportedSystems ? [ "x86_64-linux" ]
}:

with pkgs;

 let
  forAllSystems = pkgs.lib.genAttrs supportedSystems;

  importTest = fn: args: system: let
    imported = import fn;
    test = import (pkgs.path + "/nixos/tests/make-test-python.nix") imported;
  in test ({
    inherit pkgs system config;
  } // args);

  callTest = fn: args: forAllSystems (system: let test = importTest fn args system; in { inherit test; });

in {
  submit-vote = callTest ./submit-vote.nix {};
}
