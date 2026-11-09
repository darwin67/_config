{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  fastmail-cli = pkgs.callPackage ../../pkgs/fastmail-cli.nix { };
  omarchy-pods-daemon = pkgs.callPackage ../../pkgs/omarchy-pods-daemon.nix {
    src = inputs.omarchy-pods;
  };
  # Exports API keys decrypted by sops-nix (see the host's secrets.enc.yaml) so
  # they stay out of git, the Nix store and ~/.config/ai-usagebar.
  ai-usagebar =
    let
      keys = {
        anthropic-admin-key = "ANTHROPIC_ADMIN_KEY";
        anthropic-admin-key-summit = "ANTHROPIC_ADMIN_KEY_SUMMIT";
        ollama-api-key = "OLLAMA_API_KEY";
        deepseek-api-key = "DEEPSEEK_API_KEY";
        opencode-go-api-key = "OPENCODE_GO_API_KEY";
      };
      exports = lib.concatStringsSep " " (
        lib.mapAttrsToList (
          secret: var: "--run 'k=/run/secrets/${secret}; [ -r \"$k\" ] && export ${var}=\"$(<\"$k\")\"'"
        ) keys
      );
    in
    pkgs.symlinkJoin {
      name = "ai-usagebar";
      paths = [ inputs.ai-usagebar.packages.${pkgs.stdenv.hostPlatform.system}.default ];
      nativeBuildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        for bin in $out/bin/*; do
          wrapProgram "$bin" ${exports}
        done
      '';
    };
  # Month-to-date OpenAI Platform spend for the local openai-usage Noctalia
  # plugin; ai-usagebar reads OpenAI usage only from Codex logins.
  openai-usage = pkgs.writeShellApplication {
    name = "openai-usage";
    runtimeInputs = with pkgs; [
      coreutils
      curl
      jq
    ];
    text = ''
      # Costs come in daily UTC buckets, so the month starts at UTC midnight.
      start=$(date -u -d "$(date -u +%Y-%m-01)" +%s)
      # The key goes through stdin so it never appears in a process's arguments.
      total=$(
        printf 'Authorization: Bearer %s\n' "$(< /run/secrets/openai-admin-key)" |
          curl --silent --show-error --fail --max-time 20 --header @- \
            "https://api.openai.com/v1/organization/costs?start_time=$start&bucket_width=1d&limit=31" |
          jq '[.data[].results[].amount.value // 0] | add // 0'
      )
      text=$(printf "\$%.2f" "$total")
      jq -nc --arg text "$text" --arg since "$(date -u +'%b 1')" \
        '{text: $text, tooltip: "OpenAI API: \($text) this month, since \($since) UTC"}'
    '';
  };

  editor = with pkgs; [
    vscode

    # editor utilities
    glslang

    # org mode
    maim
    scrot
    gnuplot

    # LSPs
    vimPlugins.vim-lsp
  ];

  apps = with pkgs; [
    signal-desktop
    slack
    zapzap
    # wechat-uos
    discord
    zulip
    nordic
    _1password-gui
    _1password-cli # cli tool
    mpv
    yt-dlp
    spotify
    obsidian
    libreoffice-still
    dropbox
    zoom-us
  ];

  sysutils = with pkgs; [
    pulseaudio
    pavucontrol # per-device audio profiles, e.g. switching a headset to HFP
    alacritty
    thunar
    seahorse
    kitty
    flatpak
    ghostty
    gcr_4
    fontconfig
  ];

  desktop = with pkgs; [
    # Niri session
    (inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
      # Hide nearby Bluetooth devices that broadcast no name (shown as bare addresses).
      patches = (old.patches or [ ]) ++ [ ../../pkgs/noctalia-hide-nameless-bluetooth.patch ];
    }))
    xwayland-satellite
    swayidle # idle lock and screen-off, started from the Niri config
    wl-clipboard
    wf-recorder
    slurp # region picker for Noctalia's Screen Recorder plugin
    rtkit # for pipewire
    polkit # for 1password
    polkit_gnome
    playerctl
    brightnessctl
    bluez
    pbpctrl # Pixel Buds Pro controls for the Noctalia Pixel Buds plugin
    omarchy-pods-daemon # AirPods daemon + librepods-ctl for the Noctalia AirPods plugin
    ai-usagebar # AI plan and API usage for the Noctalia AI Usage plugin
    openai-usage
    bluez-tools

    fwupd # hardware
    tlp # battery management
    inotify-tools
    graphviz
    libxml2
    libayatana-appindicator

    # Amp runner's private Linux desktop; Waymote binaries are downloaded by Amp.
    labwc
    wlr-randr
    ffmpeg
  ];

  browser = with pkgs; [
    google-chrome
    brave
  ];

  utils = with pkgs; [
    fastmail-cli

    # scanner
    simple-scan
  ];

in
{
  environment.systemPackages = sysutils ++ desktop ++ editor ++ browser ++ apps ++ utils;

  # The package ships the sandboxed AirPods daemon unit; start it with the session.
  systemd.packages = [ omarchy-pods-daemon ];
  systemd.user.services.librepods.wantedBy = [ "graphical-session.target" ];
}
