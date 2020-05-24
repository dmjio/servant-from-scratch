{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (p: with p; [ aeson aeson-pretty http-client-tls http-client warp wai http-api-data]);
  build = pkgs.stdenv.mkDerivation {
   name = "servant-from-scratch";
   src = ./.;
   buildCommand = ''
     mkdir -p $out/{bin,src}
     cp $src/Main.lhs Main.lhs
     ${ghc}/bin/ghc Main.lhs -o main
     mv main $out/bin
     mv Main.lhs $out/src
   '';
  };
in
{ inherit build ghc;
}
