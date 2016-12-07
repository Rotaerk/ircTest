{ mkDerivation, attoparsec, base, bytestring, irc, network-simple
, pipes, pipes-attoparsec, pipes-network, pipes-parse, pipes-safe
, stdenv, transformers
}:
mkDerivation {
  pname = "ircTest";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring irc network-simple pipes
    pipes-attoparsec pipes-network pipes-parse pipes-safe transformers
  ];
  description = "A spike solution testing interaction with an IRC server from haskell using pipes";
  license = stdenv.lib.licenses.bsd3;
}
