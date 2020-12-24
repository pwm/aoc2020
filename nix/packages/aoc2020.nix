{ mkDerivation, base, bytestring, containers, cookie, data-clist
, directory, either, extra, generic-lens, hashtables, hpack, hspec
, hspec-discover, http-client, HUnit, lens, logict, massiv
, megaparsec, mtl, optparse-applicative, parser-combinators
, primitive, protolude, QuickCheck, quickcheck-instances, req
, split, stdenv, text, time, vector
}:
mkDerivation {
  pname = "aoc2020";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring containers cookie data-clist directory either extra
    generic-lens hashtables http-client lens logict massiv megaparsec
    mtl optparse-applicative parser-combinators primitive protolude req
    split text time vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring containers cookie data-clist directory either extra
    generic-lens hashtables http-client lens logict massiv megaparsec
    mtl optparse-applicative parser-combinators primitive protolude req
    split text time vector
  ];
  testHaskellDepends = [
    base bytestring containers cookie data-clist directory either extra
    generic-lens hashtables hspec http-client HUnit lens logict massiv
    megaparsec mtl optparse-applicative parser-combinators primitive
    protolude QuickCheck quickcheck-instances req split text time
    vector
  ];
  testToolDepends = [ hspec-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/pwm/aoc2020#readme";
  license = stdenv.lib.licenses.bsd3;
}
