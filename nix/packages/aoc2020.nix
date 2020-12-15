{ mkDerivation, base, bytestring, containers, cookie, directory
, either, generic-lens, hashtables, hpack, hspec, hspec-discover
, http-client, HUnit, lens, massiv, megaparsec, mtl
, optparse-applicative, primitive, protolude, QuickCheck
, quickcheck-instances, req, split, stdenv, text, time
}:
mkDerivation {
  pname = "aoc2020";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring containers cookie directory either generic-lens
    hashtables http-client lens massiv megaparsec mtl
    optparse-applicative primitive protolude req split text time
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring containers cookie directory either generic-lens
    hashtables http-client lens massiv megaparsec mtl
    optparse-applicative primitive protolude req split text time
  ];
  testHaskellDepends = [
    base bytestring containers cookie directory either generic-lens
    hashtables hspec http-client HUnit lens massiv megaparsec mtl
    optparse-applicative primitive protolude QuickCheck
    quickcheck-instances req split text time
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/pwm/aoc2020#readme";
  license = stdenv.lib.licenses.bsd3;
}
