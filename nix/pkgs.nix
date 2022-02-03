# our packages overlay
pkgs: prev: with pkgs;
  let
    compiler = config.haskellNix.compiler or "ghc8107";
  in {
  votingToolsHaskellProject = import ./haskell.nix {
    inherit
      haskell-nix
      lib
      ;
  };
  votingToolsHaskellPackages = import ./haskell.nix {
    inherit
      haskell-nix
      lib
      ;
  };

   jormungandr = let
    jormungandr-src = pkgs.fetchurl {
      url =
        "https://github.com/input-output-hk/jormungandr/releases/download/v0.9.3/jormungandr-0.9.3-x86_64-unknown-linux-musl-generic.tar.gz";
      sha256 = "sha256:14giz9yz94mdjrdr96rz5xsj21aacdw8mqrfdz031czh4qgnmnzh";
    };
     in pkgs.runCommand "jormungandr" { buildInputs = [ pkgs.gnutar ]; } ''
       mkdir -p $out/bin
       cd $out/bin
       tar -zxvf ${jormungandr-src}
     '';

  # systemd can't be statically linked:
  postgresql = (prev.postgresql_11
    .overrideAttrs (_: { dontDisableStatic = stdenv.hostPlatform.isMusl; }))
    .override {
      enableSystemd = stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isMusl;
      gssSupport = stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isMusl;
    };


}
