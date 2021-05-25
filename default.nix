{ compiler ? "ghc8104" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "gf-lsp" =
        hself.callCabal2nix
          "gf-lsp"
          (gitignore ./.)
          { };
      gf = pkgs.haskell.lib.overrideCabal (hself.callCabal2nix "gf" sources.gf-core { }) (
        _old: {
          # Fix utf8 encoding problems
          patches = [
            (
              pkgs.fetchpatch {
                url = "https://github.com/anka-213/gf-core/commit/6f1ca05fddbcbc860898ddf10a557b513dfafc18.patch";
                sha256 = "17vn3hncxm1dwbgpfmrl6gk6wljz3r28j191lpv5zx741pmzgbnm";
              }
            )
            ./nix/expose-all.patch
          ];
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
