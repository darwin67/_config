{ stdenv, fetchurl, pkgs, cfg, ... }:

let
  source = {
    Mojave = {
      path = "f8jzf4ugy8/Mojave%20XP.heic";
      sha256 = "sha256-Uuv/f9+z4ERJXImEjwfW4DgYSrdmVcubon6B1vBFuvE=";
    };
    Earth = {
      path = "hqzkn6nai0f/earth%20.heic";
      sha256 = "sha256-r2KlV/IN6Mcutj6h0cdu8HjQ10deoU0OTxJaXzlPMnY=";
    };
    Catalina = {
      path = "v5y04cx6k9k/Catalina.heic";
      sha256 = "sha256-3rjL308rwEj0m7G006yOvzvij928Az9HX1Kq8uHj46Y=";
    };
    macBigSur = {
      path = "boc339cw3n/Big%20macOS%20Sur.heic";
      sha256 = "sha256-wX8DLV+DWSev4wyCqdmqI4fSm1ROylm4nCuHMd85ZIw=";
    };
    macMonterey = {
      path = "la4wfuwtkg/macOS%20Monterey.heic";
      sha256 = "sha256-x2Awte2XeBpnrg4snFheVIcp91IM80/jUWyjaSm1h44=";
    };
  };

  # wallutils helper script to convert .heic format wallpapers
  heic-install = stdenv.mkDerivation {
    name = "heic-install";
    src = ./heic-install;

    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/heic-install
      sed -i -e 's/\r$//' $out/bin/heic-install
      chmod +x $out/bin/heic-install
    '';
  };

  wallpaper = source.${cfg.theme};

in stdenv.mkDerivation {
  name = "timed-wallpaper";
  src = fetchurl {
    name = cfg.theme;
    url = "https://cdn.dynamicwallpaper.club/wallpapers/${wallpaper.path}";
    sha256 = wallpaper.sha256;
  };
  nativeBuildInputs = [ pkgs.wallutils pkgs.imagemagick heic-install ];

  phases = [ "installPhase" ];
  installPhase = ''
    FILENAME=${cfg.theme}.heic
    cp $src $FILENAME

    # Try running with bash explicitly
    bash ${heic-install}/bin/heic-install $FILENAME $out
  '';
}
