{ mkDerivation, base, hakyll, pandoc, stdenv }:
mkDerivation {
  pname = "blog";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll pandoc ];
  license = stdenv.lib.licenses.unfree;
}
