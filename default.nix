{ mkDerivation, stdenv, base, containers, megaparsec, text, vector
, devDeps ? [ ]
, devSystemDeps ? [ ]
}:
mkDerivation {
  pname = "advent-of-code-2020";
  version = "0.1.0.0";
  src= ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = devSystemDeps;
  libraryHaskellDepends = [
    base containers megaparsec text
  ] ++ devDeps;
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/agnantis/advent-of-code-2020";
  license = stdenv.lib.licenses.bsd3;
}
