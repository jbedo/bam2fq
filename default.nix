{ stdenv, haskell, ghc }:

let
  haskenv = with haskell.lib;
    ghc.withPackages (pkgs:
      with pkgs; [
        optparse-applicative
        streaming-with
        streaming-utils
        streaming-bytestring
        (doJailbreak (markUnbroken
          (biohazard.overrideAttrs (_: { patches = [ ./biohazard.patch ]; }))))
      ]);
in stdenv.mkDerivation {
  name = "bam2fq";
  buildInputs = [ haskenv ];
  src = ./.;
  buildPhase = ''
    ghc -O3 bam2fq.hs -o bam2fq
  '';
  installPhase = ''
    install -D bam2fq $out/bin/bam2fq
  '';
}
