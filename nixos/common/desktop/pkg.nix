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
