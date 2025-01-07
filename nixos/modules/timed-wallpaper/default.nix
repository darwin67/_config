{ stdenv, fetchurl, pkgs, cfg, ... }:

let
  source = {
    DesertSands = "a9q1jiy0cu/%22Desert%20Sands%22%20by%20Louis%20Coyle.heic";
    Mojave = "f8jzf4ugy8/Mojave%20XP.heic";
    Earth = "hqzkn6nai0f/earth%20.heic";
    TechFactory = "ze2isquaf7n/Tech%20Factory.heic";
    Catalina = "v5y04cx6k9k/Catalina.heic";
    macBigSur = "boc339cw3n/Big%20macOS%20Sur.heic";
    macMonterey = "la4wfuwtkg/macOS%20Monterey.heic";
    Fuji = "gpf7f97jk3b/Fuji.heic";
  };

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

  wallpaper-path = source.${cfg.theme};

in stdenv.mkDerivation {
  name = "timed-wallpaper";
  src = fetchurl {
    name = "wallpaper";
    url = "https://cdn.dynamicwallpaper.club/wallpapers/${wallpaper-path}";
    sha256 = "13l7nllnk8vca7ilzwqcabvjk1slbrc9qb0fmrkily4pxnsk0q67";
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
