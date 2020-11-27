{ mkDerivation, stdenv, base, containers, megaparsec, text
, devDeps ? [ ]
, devSystemDeps ? [ ]
}:
mkDerivation {
  pname = "advent-of-code-2019";
  version = "0.1.0.0";
  src= ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = devSystemDeps;
  libraryHaskellDepends = [
    base containers megaparsec text
  ] ++ devDeps;
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/agnantis/advent-of-code-2019";
  license = stdenv.lib.licenses.bsd3;
}
