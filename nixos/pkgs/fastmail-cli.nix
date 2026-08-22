{
  lib,
  stdenvNoCC,
  fetchurl,
  autoPatchelfHook,
  stdenv,
}:

let
  version = "3.4.0";
  sources = {
    x86_64-linux = {
      platform = "linux-x86_64";
      hash = "sha256-o162wmEqKnb7K1M1izSCVYw1MO6lfG0vXlLShrBJ7o8=";
    };
    aarch64-linux = {
      platform = "linux-aarch64";
      hash = "sha256-/+4cghoADxJq4LAJzx5RTZXAs57zKC1HJ6XOAlPoWk4=";
    };
    x86_64-darwin = {
      platform = "darwin-x86_64";
      hash = "sha256-I6bOchWhIIPno5JUPmzz3OihgU0dZGVDleRJGhi+C8M=";
    };
    aarch64-darwin = {
      platform = "darwin-aarch64";
      hash = "sha256-4NVYs466847DQBXtA1aHHvBttSXyqco+sAWQHqlMmWc=";
    };
  };
  source = sources.${stdenv.hostPlatform.system};
in
stdenvNoCC.mkDerivation {
  pname = "fastmail-cli";
  inherit version;

  src = fetchurl {
    url = "https://github.com/radiosilence/fastmail-cli/releases/download/v${version}/fastmail-cli-${source.platform}.tar.gz";
    inherit (source) hash;
  };

  dontUnpack = true;

  nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];
  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [
    stdenv.cc.cc.lib
    stdenv.cc.libc
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    tar -xzf $src -C $out/bin

    runHook postInstall
  '';

  meta = {
    description = "CLI for Fastmail's JMAP API";
    homepage = "https://github.com/radiosilence/fastmail-cli";
    license = lib.licenses.mit;
    mainProgram = "fastmail";
    platforms = builtins.attrNames sources;
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
}
