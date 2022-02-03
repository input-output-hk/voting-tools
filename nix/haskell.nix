############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, lib
}:

let
  inherit (haskell-nix) haskellLib;

  src = haskellLib.cleanSourceWith {
    src = ../.;
    name = "voting-tools-src";
    filter = name: type: (lib.cleanSourceFilter name type)
      && (haskell-nix.haskellSourceFilter name type)
      # removes socket files
      && lib.elem type [ "regular" "directory" "symlink" ];
  };
  compiler-nix-name = "ghc8107";
  cabalProjectLocal = ''
    allow-newer: terminfo:base
  '';

  projectPackages = lib.attrNames (haskellLib.selectProjectPackages
    (haskell-nix.cabalProject {
      inherit src compiler-nix-name cabalProjectLocal;
    }));

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject' ({ pkgs
   , config
   , buildProject
   , ...
   }:{
    inherit src compiler-nix-name cabalProjectLocal;

    # This provides a development environment that can be used with nix-shell or
    # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
    shell = {
      name = "voting-tools-shell";

      # If shellFor local packages selection is wrong,
      # then list all local packages then include source-repository-package that cabal complains about:
      packages = ps: lib.attrValues (haskellLib.selectProjectPackages ps);

      # These programs will be available inside the nix-shell.
      nativeBuildInputs = with buildProject.hsPkgs; [
          bech32.components.exes.bech32
        ] ++ (with pkgs.buildPackages.buildPackages; [
        git
        ghcid
        hlint
        pkgconfig
        stylish-haskell
        jormungandr
        cabalWrapped
        # we also add cabal (even if cabalWrapped will be used by default) for shell completion:
        cabal-install
      ]);

      # Prevents cabal from choosing alternate plans, so that
      # *all* dependencies are provided by Nix.
      exactDeps = true;

      withHoogle = true;

      GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    };
    modules = [
      # Allow reinstallation of Win32
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isWindows {
       nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      })
      ({ pkgs, ...}: {
        # Use the VRF fork of libsodium
        packages = lib.genAttrs [ "cardano-crypto-praos" "cardano-crypto-class" ] (_: {
          components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        });
      })
      ({ pkgs, ...}: {
        # make sure that libsodium DLLs are available for windows binaries:
        packages = lib.genAttrs projectPackages (name: {
          postInstall = lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
            if [ -d $out/bin ]; then
              ${setLibSodium pkgs}
            fi
          '';
        });
      })
      ({config, pkgs, ...}: {
        # Stamp executables with the git revision
        packages = lib.genAttrs ["cardano-node" "cardano-cli"] (name: {
          components.exes.${name}.postInstall = ''
            ${lib.optionalString pkgs.stdenv.hostPlatform.isWindows (setLibSodium pkgs)}
            ${setGitRev pkgs config.packages.cardano-node.src.rev}
          '';
        });
      })
      ({ pkgs, config, ... }: {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;

      })
      {
        packages = lib.genAttrs projectPackages
          (name: { configureFlags = [ "--ghc-option=-Werror" ]; });
      }
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
        # systemd can't be statically linked
        packages.cardano-config.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;
        packages.cardano-node.flags.systemd = !pkgs.stdenv.hostPlatform.isMusl;

        # FIXME: Error loading shared library libHSvoting-tools-0.2.0.0-HDZeaOp1VIwKhm4zJgwaOj.so: No such file or directory
        packages.voting-tools.components.tests.unit-tests.buildable = lib.mkForce (!pkgs.stdenv.hostPlatform.isMusl);
      })
      # Musl libc fully static build
      ({ pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isMusl (let
        # Module options which adds GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = false;
          enableStatic = true;
          configureFlags = [
            "--ghc-option=-optl=-lssl"
            "--ghc-option=-optl=-lcrypto"
            "--ghc-option=-optl=-L${pkgs.openssl.out}/lib"
          ];
        };
      in
        {
          packages = lib.genAttrs projectPackages (name: fullyStaticOptions);

          # Haddock not working and not needed for cross builds
          doHaddock = false;
        }
      ))

      ({ pkgs, ... }: lib.mkIf (pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform) {
        # Remove hsc2hs build-tool dependencies (suitable version will be available as part of the ghc derivation)
        packages.Win32.components.library.build-tools = lib.mkForce [];
        packages.terminal-size.components.library.build-tools = lib.mkForce [];
        packages.network.components.library.build-tools = lib.mkForce [];
      })
    ];
  });
  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  setGitRev = pkgs: rev: ''${pkgs.buildPackages.haskellBuildUtils}/bin/set-git-rev "${rev}" $out/bin/*'';
  # package with libsodium:
  setLibSodium = pkgs : "ln -s ${pkgs.libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll";
in
  pkgSet
