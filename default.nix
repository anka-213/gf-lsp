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
              # Don't statically link libiconv since it uses data files
              # "--extra-lib-dirs=${stripDylib (pkgs.libiconv.override { enableStatic = true; enableShared = false; })}/lib"
              "--extra-lib-dirs=${stripDylib (libffi.overrideAttrs (_: { dontDisableStatic = true; }))}/lib"
              # Don't statically link ncurses since it uses data files
              # "--extra-lib-dirs=${stripDylib (ncurses.override { enableStatic = true; })}/lib"
            ]
            ++ lib.optionals stdenv.isLinux [
              "--enable-executable-static"
              # TODO: replace this with musl: https://stackoverflow.com/a/57478728
              "--extra-lib-dirs=${glibc}/lib"
              "--extra-lib-dirs=${glibc.static}/lib"
            ]
          ))
          haskell.lib.dontHaddock
        ]);
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
              # (pkgs.ncurses.override { enableStatic = true; })
              # # (pkgs.ncurses)
              # (pkgs.libiconv.override { enableStatic = true; enableShared = false; })
              # (pkgs.gmp.override { withStatic = true; })
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

  # Based on this: https://github.com/ethereum/hevm/pull/185
  # "static" binary for distribution
  # on linux this is actually a real fully static binary
  # on macos this has everything except libsystem, libncurses and libiconv
  # statically linked. we can be confident that these three will always
  # be provided in a well known location by macos itself.
  gf-lsp-redistributable =
    let
      grep = "${pkgs.gnugrep}/bin/grep";
      # awk = "${pkgs.awk}/bin/awk"; # Already included in stdenv
      otool = "${pkgs.darwin.binutils.bintools}/bin/otool";
      install_name_tool = "${pkgs.darwin.binutils.bintools}/bin/install_name_tool";
    in
    if pkgs.stdenv.isLinux
    then pkgs.haskell.lib.dontCheck myHaskellPackages.gf-lsp-static
    else
      pkgs.runCommand "stripNixRefs" { } ''
        # Find the rpaths of an executable
        lsrpath() {
            ${otool} -l "$@" |
            awk '
                /^[^ ]/ {f = 0}
                $2 == "LC_RPATH" && $1 == "cmd" {f = 1}
                f && gsub(/^ *path | \(offset [0-9]+\)$/, "") == 2
            '
        }
        mkdir -p $out/bin
        cp ${pkgs.haskell.lib.dontCheck myHaskellPackages.gf-lsp-static}/bin/gf-lsp $out/bin/
        # get the list of dynamic libs from otool and tidy the output
        libs=$(${otool} -L $out/bin/gf-lsp | tail -n +2 | sed 's/^[[:space:]]*//' | cut -d' ' -f1)
        # get the paths for libncurses and libiconv
        ncurses=$(echo "$libs" | ${grep} '^/nix/store/.*-ncurses')
        iconv=$(echo "$libs" | ${grep} '^/nix/store/.*-libiconv')
        # Get the rpath for the CoreFoundation.framework
        rpath=$(lsrpath $out/bin/gf-lsp)
        # rewrite /nix/... library paths to point to /usr/lib
        chmod 777 $out/bin/gf-lsp
        ${install_name_tool} -change "$ncurses" /usr/lib/libncurses.dylib $out/bin/gf-lsp
        ${install_name_tool} -change "$iconv" /usr/lib/libiconv.dylib $out/bin/gf-lsp
        ${install_name_tool} -rpath "$rpath" /System/Library/Frameworks $out/bin/gf-lsp
        chmod 555 $out/bin/gf-lsp
      '';

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
  inherit gf-lsp-redistributable;
  inherit docker;
  inherit myHaskellPackages;
  "gf-lsp" = myHaskellPackages."gf-lsp";
  "gf-lsp-static" = myHaskellPackages."gf-lsp-static";
}
