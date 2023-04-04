{ pkgs }:
let
  inherit (pkgs) haskell cabal2nix cabal-install ghcid;

  haskellPackages = haskell.packages.ghc902;

  project = import ./default.nix { inherit pkgs; };

in
with pkgs;
mkShell {
  buildInputs = project.env.nativeBuildInputs ++ [
    cabal-install
    cabal2nix
    ghcid
  ];

  LOCALE_ARCHIVE = lib.optionalString stdenv.isLinux
    "${pkgs.glibcLocales}/lib/locale/locale-archive";
}
