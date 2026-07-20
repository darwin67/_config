{
  config,
  lib,
  pkgs,
  ...
}:

let
  gpg = "${config.programs.gpg.package}/bin/gpg";
  grep = "${pkgs.gnugrep}/bin/grep";
  cut = "${pkgs.coreutils}/bin/cut";
  head = "${pkgs.coreutils}/bin/head";
in
{
  programs.gpg = {
    enable = true;
    mutableKeys = true;
    mutableTrust = true;

    settings = {
      keyserver = "hkps://keys.openpgp.org";
      auto-key-locate = "local,wkd,keyserver";
      auto-key-retrieve = true;
    };
  };

  services.gpg-agent = {
    enable = true;
    enableZshIntegration = true;
    defaultCacheTtl = 1800;
    maxCacheTtl = 7200;
    pinentry.package = lib.mkDefault pkgs.pinentry-curses;
  };

  home.activation.importSopsGpgPrivateKey = lib.hm.dag.entryAfter [ "createGpgHomedir" ] ''
    secret="/run/secrets/gpg-private-key"

    if [ -r "$secret" ]; then
      export GNUPGHOME=${lib.escapeShellArg config.programs.gpg.homedir}
      fingerprint="$(${gpg} --show-keys --with-colons "$secret" | ${grep} '^fpr:' | ${cut} -d: -f10 | ${head} -n1 || true)"

      if [ -n "$fingerprint" ] && ! ${gpg} --list-secret-keys --with-colons "$fingerprint" >/dev/null 2>&1; then
        run ${gpg} --batch --import "$secret"
      fi

      unset GNUPGHOME fingerprint
    fi
  '';
}
