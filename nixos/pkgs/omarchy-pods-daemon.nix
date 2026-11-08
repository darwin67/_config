# The librepods fork from omarchy-pods: the AirPods daemon that the
# harveywuk/airpods Noctalia plugin reads (status file, `status` and control
# verbs). Upstream librepods in nixpkgs lacks those.
{
  lib,
  stdenv,
  src,
  cmake,
  ninja,
  pkg-config,
  qt6,
  openssl,
  libpulseaudio,
}:

stdenv.mkDerivation {
  pname = "omarchy-pods-daemon";
  version = "0-unstable-${src.lastModifiedDate or "unknown"}";

  inherit src;
  sourceRoot = "source/daemon";

  nativeBuildInputs = [
    cmake
    ninja
    pkg-config
    qt6.wrapQtAppsHook
    qt6.qttools
  ];

  buildInputs = [
    qt6.qtbase
    qt6.qtdeclarative
    qt6.qtconnectivity
    openssl
    libpulseaudio
  ];

  cmakeFlags = [ "-DBUILD_TESTING=OFF" ];

  # The unit expects a ~/.local install; NixOS collects units from lib/systemd.
  postInstall = ''
    mkdir -p $out/lib/systemd
    mv $out/share/systemd/user $out/lib/systemd/user
    substituteInPlace $out/lib/systemd/user/librepods.service \
      --replace-fail '%h/.local/bin/librepods' "$out/bin/librepods"
  '';

  meta = {
    description = "AirPods daemon (librepods fork) for the omarchy-pods bar widget";
    homepage = "https://github.com/thisisgm/omarchy-pods";
    license = lib.licenses.gpl3Only;
    mainProgram = "librepods";
    platforms = lib.platforms.linux;
  };
}
