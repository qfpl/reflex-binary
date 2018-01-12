{ reflex-platform ? import ./reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  drv = ghc.callPackage ./reflex-binary.nix {};
in
  drv
