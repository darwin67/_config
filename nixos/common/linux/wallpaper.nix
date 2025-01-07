{ theme, pkgs, ... }:

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

  wallpaper = source.${theme};
in pkgs.stdenv.mkDerivation {
  name = "timed-wallpaper";
  src = builtins.fetchurl {
    name = theme;
    url = "https://cdn.dynamicwallpaper.club/wallpapers/${wallpaper.path}";
    sha256 = wallpaper.sha256;
  };

  phases = [ "installPhase" ];
  installPhase = ''
    cp $src $out
  '';
}
