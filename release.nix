############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ voting-tools ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    inherit sourcesOverride;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }
}:

with (import pkgs.commonLib.release-lib) {
  inherit pkgs;

  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import voting-tools;
  gitrev = voting-tools.rev;
};

with pkgs.lib;

let
  testsSupportedSystems = [ "x86_64-linux" ];
  collectTests = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);

  inherit (systems.examples) musl64;

  jobs = {
    native = mapTestOn (packagePlatforms project);
  } // (mkRequiredJob (
      collectTests jobs.native.checks.tests ++
      collectTests jobs.native.benchmarks ++
      [
        jobs.native.voter-registration.x86_64-linux
        jobs.native.voting-tools.x86_64-linux
        jobs.native.voterRegistrationTarball.x86_64-linux
      ]
    ))
  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms { inherit (project) shell; });
in jobs
