{ mkDerivation, base, hakyll, pandoc, stdenv }:
mkDerivation {
  pname = "blog";
  version = "0.1.0";
  src = builtins.filterSource (path: type:
          baseNameOf path != "result"  &&
          baseNameOf path != "posts"   &&
          baseNameOf path != "images"  &&
          baseNameOf path != "_site"   &&
          baseNameOf path != "_cache"  &&
          baseNameOf path != ".travis" &&
          baseNameOf path != ".git") ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base hakyll pandoc ];
  license = stdenv.lib.licenses.bsd3;
}
