{
  lib,
  stdenvNoCC,
  fetchurl,
}:

stdenvNoCC.mkDerivation rec {
  pname = "rift";
  version = "0.8.3";

  src = fetchurl {
    url = "https://github.com/acsandmann/rift/releases/download/v${version}/rift-app-universal-macos-${version}.tar.gz";
    hash = "sha256-iyyFJpR/xetw95D7By5ccl4AwgfW9yZvWE9KVFXIJyo=";
  };

  dontUnpack = true;
  # Preserve the upstream app and CLI signatures for macOS Accessibility identity.
  dontFixup = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/Applications $out/bin $out/share/rift
    tar -xzf $src
    cp -R Rift.app $out/Applications/
    cp rift-cli $out/bin/
    cp rift.default.toml $out/share/rift/
    ln -s $out/Applications/Rift.app/Contents/MacOS/rift $out/bin/rift

    runHook postInstall
  '';

  meta = {
    description = "Tiling window manager for macOS with Niri-style scrolling columns";
    homepage = "https://github.com/acsandmann/rift";
    license = lib.licenses.asl20;
    platforms = lib.platforms.darwin;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    mainProgram = "rift";
  };
}
