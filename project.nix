{ mkDerivation, base, QuickCheck, stdenv, test-framework
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "gray-extended";
  version = "1.5.8";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base QuickCheck test-framework test-framework-quickcheck2
  ];
  enableLibraryProfiling = true;
  enableExecutableProfiling = true;
  homepage = "https://github.com/mhwombat/gray-extended";
  description = "Gray encoding schemes";
  license = stdenv.lib.licenses.bsd3;
}
