{}:
with import <nixpkgs> {};
let
  inherit (haskellPackages) cabal cabalInstall lens linear monadLoops;

  SDL2 = haskellPackages.callPackage /home/ollie/work/hsSDL2 {};
  netwire = import /home/ollie/work/netwire {};

in cabal.mkDerivation (self: {
  pname = "mario";
  version = "1";
  src = ./.;
  buildDepends = [ lens linear monadLoops mesa netwire.netwire SDL2 ];
  buildTools = [ cabalInstall ];
})