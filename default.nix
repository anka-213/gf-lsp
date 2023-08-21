{ system ? builtins.currentSystem, compiler ? "ghc92" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit system; };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ "nix\n*.nix\n.github\n.git\n" ./.nixignore ./.gitignore ]; # ./.git/info/exclude


  hlib = pkgs.haskell.lib;

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "gf-lsp-static" = (with pkgs; lib.pipe
        (hself.callCabal2nix
          "gf-lsp"
          (gitignore ./.)
          {
            # ncurses = (pkgs.ncurses.override { enableStatic = true; });
          })
        # ( haskellPackages.callCabal2nix "hevm" ./. {
        #     # Haskell libs with the same names as C libs...
        #     # Depend on the C libs, not the Haskell libs.
        #     # These are system deps, not Cabal deps.
        #     inherit secp256k1;
        #   }
        # )
        [
          # (haskell.lib.compose.overrideCabal (old: { testTarget = "test"; }))
          # (haskell.lib.compose.addTestToolDepends [ solc z3 cvc5 ])
          (haskell.lib.compose.appendBuildFlags [ "-v3" ])
          (haskell.lib.compose.appendConfigureFlags (
            [
              # "-fci"
              "--extra-lib-dirs=${stripDylib (pkgs.gmp.override { withStatic = true; })}/lib"
              # "--extra-lib-dirs=${stripDylib secp256k1-static}/lib"
              # "--extra-lib-dirs=${stripDylib (libff.override { enableStatic = true; })}/lib"
              # "--extra-lib-dirs=${zlib.static}/lib"
              "--extra-lib-dirs=${stripDylib (pkgs.libiconv.override { enableStatic = true; enableShared = false; })}/lib"
              "--extra-lib-dirs=${stripDylib (libffi.overrideAttrs (_: { dontDisableStatic = true; }))}/lib"
              "--extra-lib-dirs=${stripDylib (ncurses.override { enableStatic = true; })}/lib"
            ]
            ++ lib.optionals stdenv.isLinux [
              "--enable-executable-static"
              # TODO: replace this with musl: https://stackoverflow.com/a/57478728
              "--extra-lib-dirs=${glibc}/lib"
              "--extra-lib-dirs=${glibc.static}/lib"
            ]
          ))
          haskell.lib.dontHaddock
        ]).overrideAttrs (final: prev: {
        # HEVM_SOLIDITY_REPO = solidity;
        # HEVM_ETHEREUM_TESTS_REPO = ethereum-tests;
      });
      "gf-lsp" =
        pkgs.haskell.lib.overrideCabal
          (hself.callCabal2nix
            "gf-lsp"
            (gitignore ./.)
            {
              # ncurses = (pkgs.ncurses.override { enableStatic = true; });
            })
          {
            executableSystemDepends = [
              (pkgs.ncurses.override { enableStatic = true; })
              # (pkgs.ncurses)
              (pkgs.libiconv.override { enableStatic = true; enableShared = false; })
              (pkgs.gmp.override { withStatic = true; })
            ];
          };
      "co-log-concurrent" = hlib.overrideCabal (hlib.unmarkBroken hsuper.co-log-concurrent)
        (_old: {
          src = sources.co-log-concurrent;
          # patches = [
          #   (
          #     # Support for ghc-9.6
          #     pkgs.fetchpatch {
          #       url = "https://github.com/qnikst/co-log-concurrent/commit/a3f6fa4958493737270550b20b09db847ec2aecb.patch";
          #       sha256 = "sha256-q+eVl8cip0hj4Pd5CjVwgV1UfmB2rLur5in91IkSVIU=";
          #     }
          #   )
          # ];
        });
      gf = hlib.overrideCabal
        (
          # pkgs.haskell.lib.disableCabalFlag
          (hself.callCabal2nixWithOptions "gf"
            sources.gf-core "--flag=-server"
            { })
          # "server"
        )
        (
          _old: {
            # Fix utf8 encoding problems
            patches = [
              # Already applied in master
              # (
              #   pkgs.fetchpatch {
              #     url = "https://github.com/anka-213/gf-core/commit/6f1ca05fddbcbc860898ddf10a557b513dfafc18.patch";
              #     sha256 = "17vn3hncxm1dwbgpfmrl6gk6wljz3r28j191lpv5zx741pmzgbnm";
              #   }
              # )
              ./nix/expose-all.patch
              ./nix/revert-new-cabal-madness.patch
            ];
            jailbreak = true;
            # executableSystemDepends = [
            #   (pkgs.ncurses.override { enableStatic = true; })
            # ];
            # executableHaskellDepends = [ ];
          }
        );
    };
  };

  shell = myHaskellPackages.shellFor
    {
      packages = p: [
        p."gf-lsp-static"
      ];
      buildInputs = [
        myHaskellPackages.haskell-language-server
        pkgs.haskellPackages.cabal-install
        pkgs.haskellPackages.ghcid
        pkgs.haskellPackages.ormolu
        pkgs.haskellPackages.hlint
        pkgs.niv
        pkgs.nixpkgs-fmt
      ];
      withHoogle = true;
    };

  exe = pkgs.haskell.lib.justStaticExecutables
    (myHaskellPackages."gf-lsp");

  exe-static = pkgs.haskell.lib.justStaticExecutables
    (myHaskellPackages."gf-lsp-static");

  docker = pkgs.dockerTools.buildImage {
    name = "gf-lsp";
    # runAsRoot = ''
    #   mkdir /tmp
    # '';
    config.Cmd = [ "${exe}/bin/gf-lsp" ];
  };

  # if we pass a library folder to ghc via --extra-lib-dirs that contains
  # only .a files, then ghc will link that library statically instead of
  # dynamically (even if --enable-executable-static is not passed to cabal).
  # we use this trick to force static linking of some libraries on macos.
  stripDylib = drv: pkgs.runCommand "${drv.name}-strip-dylibs" { } ''
    mkdir -p $out
    mkdir -p $out/lib
    cp -r ${drv}/* $out/
    rm -rf $out/**/*.dylib
  '';
in
{
  inherit shell;
  inherit exe;
  inherit exe-static;
  inherit docker;
  inherit myHaskellPackages;
  "gf-lsp" = myHaskellPackages."gf-lsp";
  "gf-lsp-static" = myHaskellPackages."gf-lsp-static";
}
