{ compiler ? "ghc884" }:
let 
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages // {
        "${compiler}" = pkgs.haskell.packages."${compiler}".override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            hs-reasoner =
              let
                devDeps = with haskellPackagesOld; if pkgs.lib.inNixShell then [ hlint ghcid doctest pkgs.gitg haskell-language-server ] else [ ];
                devSystemDeps = if pkgs.lib.inNixShell then [ pkgs.entr ] else [ ];
              in
                haskellPackagesNew.callPackage ./default.nix { inherit devDeps; inherit devSystemDeps; };
          };
        };
      };
    };
  };
  pkgs = import (import ./nix/pinned-nixpkgs.nix) { inherit config; };
in
{
  hs-reasoner = pkgs.haskellPackages.${compiler}.hs-reasoner;
}
