{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskell.packages.ghc966;
in
pkgs.mkShell {
  buildInputs = with haskellPackages; [
    ghc
    cabal-install
    haskell-language-server
  ];
  shellHook = ''
    echo "Haskell development environment loaded (GHC ${haskellPackages.ghc.version})"
  '';
}
