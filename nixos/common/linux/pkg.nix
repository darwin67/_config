{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  fastmail-cli = pkgs.callPackage ../../pkgs/fastmail-cli.nix { };

  editor = with pkgs; [
    vim
    neovim
    emacs
    vscode

    # editor utilities
    nixfmt
    editorconfig-core-c
    shfmt
    shellcheck
    glslang
    markdownlint-cli
    tree-sitter

    # org mode
    maim
    scrot
    gnuplot

    # LSPs
    vimPlugins.vim-lsp
    nixd
    vscode-json-languageserver
  ];

  apps = with pkgs; [
    signal-desktop
    slack
    karere
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
    zsh
    pulseaudio
    pavucontrol # per-device audio profiles, e.g. switching a headset to HFP
    gnumake
    gnutar
    gcc
    tmux
    wget
    xh
    git
    jujutsu
    hub
    fzf
    openssl
    alacritty
    glibcLocales
    thunar
    file
    seahorse
    kitty
    yazi
    flatpak
    zip
    unzip
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
    rtkit # for pipewire
    polkit # for 1password
    polkit_gnome
    playerctl
    brightnessctl
    bluez
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
    fastfetch
    (python313.withPackages (
      ps: with ps; [
        keyring
        pip
        grip
        pytest
        pyflakes
        isort
        cffi
        ipython
        black
      ]
    ))

    docker_29
    ctop
    ripgrep
    jq
    yq
    direnv
    nix-direnv
    fd
    bat
    sqlite
    tree
    pet
    dig
    sops
    age
    duckdb
    uv
    gh
    kubectl
    fastmail-cli

    bubblewrap # dep for codex

    # scanner
    simple-scan
  ];

in
{
  imports = [ ../llm-agents.nix ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = sysutils ++ desktop ++ editor ++ browser ++ apps ++ utils;
}
