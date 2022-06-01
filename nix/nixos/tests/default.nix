{ pkgs
, inputs
, system
}:

with pkgs;
with pkgs.commonLib;

let
  forAllSystems = genAttrs supportedSystems;
  importTest = fn: args: system: let
    imported = import fn;
    test = import (pkgs.path + "/nixos/tests/make-test-python.nix") imported;
  in test ({
    inherit pkgs system config inputs;
  } // args);
  callTest = fn: args: (importTest fn args system);
in rec {
  db-password-auth = callTest ./db-password-auth.nix {};
  mock-db = callTest ./mock-db.nix {};
  db-tests = callTest ./db-tests.nix {};
}
