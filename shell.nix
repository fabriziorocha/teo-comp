{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    cabal-install
    stack
    haskell-language-server
    hlint
    ormolu
    pkg-config
    zlib
    gmp
  ];

  shellHook = ''
    echo "Ambiente Haskell carregado (GHC/Cabal/Stack)."
    echo "Use: stack build ou cabal build"
  '';
}
