{
  lib,
  stdenv,
  fetchurl,
  dpkg,
  autoPatchelfHook,
  makeWrapper,
  alsa-lib,
  at-spi2-atk,
  cups,
  gtk3,
  libgbm,
  libGL,
  libappindicator-gtk3,
  libnotify,
  libsecret,
  libxkbcommon,
  nss,
  systemd,
  xdg-utils,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "amp-desktop";
  version = "0.1.29907961";

  # Official package from https://ampcode.com/docs/apps/linux.
  src = fetchurl {
    url = "https://static.ampcode.com/linux/${finalAttrs.version}/amp-desktop-${finalAttrs.version}-amd64.deb";
    hash = "sha256-fy9h0OdNv09T1mWXRqxuc4H9qAoDyaU4aUb3wgCzLBE=";
  };

  nativeBuildInputs = [
    dpkg
    autoPatchelfHook
    makeWrapper
  ];

  buildInputs = [
    alsa-lib
    at-spi2-atk
    cups
    gtk3
    libgbm
    libxkbcommon
    nss
    systemd
    stdenv.cc.cc.lib
  ];

  runtimeDependencies = map lib.getLib [
    libGL
    libappindicator-gtk3
    libnotify
    libsecret
  ];

  unpackPhase = ''
    runHook preUnpack
    dpkg-deb -x "$src" .
    runHook postUnpack
  '';

  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/amp-desktop $out/bin $out/share
    cp -r opt/amp/. $out/lib/amp-desktop/
    cp -r usr/share/applications usr/share/icons $out/share/

    makeWrapper $out/lib/amp-desktop/amp-desktop $out/bin/amp-desktop \
      --prefix XDG_DATA_DIRS : "$GSETTINGS_SCHEMAS_PATH" \
      --suffix PATH : ${lib.makeBinPath [ xdg-utils ]} \
      --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform-hint=auto --enable-wayland-ime=true}}"

    # Keep upstream's auth/thread URL handlers and launcher actions.
    substituteInPlace $out/share/applications/amp-desktop.desktop \
      --replace-fail /opt/amp/amp-desktop $out/bin/amp-desktop
    runHook postInstall
  '';

  meta = {
    description = "Amp desktop app for Linux";
    homepage = "https://ampcode.com/docs/apps/linux";
    license = lib.licenses.unfree;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    mainProgram = "amp-desktop";
    platforms = [ "x86_64-linux" ];
  };
})
