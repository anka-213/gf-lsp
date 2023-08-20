{ system ? builtins.currentSystem, compiler ? "ghc92" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit system; };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ "nix\n*.nix\n.github\n.git\n" ./.nixignore ./.gitignore ]; # ./.git/info/exclude

  hlib = pkgs.haskell.lib;
  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "gf-lsp" =
        hself.callCabal2nix
          "gf-lsp"
          (gitignore ./.)
          { };
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
          }
        );
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."gf-lsp"
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

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."gf-lsp");

  docker = pkgs.dockerTools.buildImage {
    name = "gf-lsp";
    # runAsRoot = ''
    #   mkdir /tmp
    # '';
    config.Cmd = [ "${exe}/bin/gf-lsp" ];
  };
in
{
  inherit shell;
  inherit exe;
  inherit docker;
  inherit myHaskellPackages;
  "gf-lsp" = myHaskellPackages."gf-lsp";
}
