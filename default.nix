{ mkDerivation, base, QuickCheck, stdenv, test-framework
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
}
