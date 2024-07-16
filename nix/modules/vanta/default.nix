{ stdenv, fetchurl, dpkg, autoPatchelfHook, zlib }:

let version = "2.0.0";

in stdenv.mkDerivation rec {
  pname = "vanta-agent";
  version = "2.8.1";

  src = fetchurl {
    url =
      "https://vanta-agent-repo.s3.amazonaws.com/targets/versions/${version}/vanta-amd64.deb";
    sha256 = "7bec7ee8f51964037f7b8ed7923e6adc3d0112ae4249eef52fef508b6d0559c5";
  };

  nativeBuildInputs = [ autoPatchelfHook zlib dpkg ];

  sourceRoot = ".";
  unpackCmd = ''dpkg-deb -x "$src" .'';

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp -a etc var $out
    cp -a usr/share/ $out/
    cp usr/lib/systemd/system/vanta.service $out/share/doc/vanta/

    mkdir -p $out/bin/;
    cp var/vanta/* $out/bin/
  '';
}
