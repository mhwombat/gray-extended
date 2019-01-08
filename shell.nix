{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, QuickCheck, stdenv, test-framework
      , test-framework-quickcheck2
      }:
      mkDerivation {
        pname = "gray-extended";
        version = "1.5.6";
        src = ./.;
        libraryHaskellDepends = [ base ];
        testHaskellDepends = [
          base QuickCheck test-framework test-framework-quickcheck2
        ];
        homepage = "https://github.com/mhwombat/gray-extended#readme";
        description = "Gray encoding schemes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
