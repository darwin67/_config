{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
  latestPkgs = import inputs.nixpkgs-dev {
    system = pkgs.stdenv.hostPlatform.system;
    config = {
      allowUnfree = true;
      allowBroken = false;
    };
  };

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
    karere
    # wechat-uos
    discord
    nordic
    _1password-gui
    _1password-cli # cli tool
    mpv
    yt-dlp
    spotify
    obsidian
    libreoffice-still
  ];

  sysutils = with pkgs; [
    zsh
    pulseaudio
    gnumake
    gnutar
    gcc
    tmux
    wget
    xh
    git
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
    sway
    swaybg
    rtkit # for pipewire
    polkit # for 1password
    polkit_gnome
    playerctl
    brightnessctl
    bluez
    bluez-tools

    # timed wallpaper
    timewall

    fwupd # hardware
    tlp # battery management
    inotify-tools
    graphviz
    libxml2
    libayatana-appindicator
  ];

  browser = with pkgs; [
    firefox-devedition
    google-chrome
    brave
  ];

  utils = with pkgs; [
    fastfetch
    i3pystatus
    (python313.withPackages (
      ps: with ps; [
        i3pystatus
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

    # Terminal AI tools
    latestPkgs.claude-code
    latestPkgs.opencode

    inputs.codex-cli-nix.packages.${pkgs.stdenv.hostPlatform.system}.default
    bubblewrap # dep for codex

    # scanner
    simple-scan
  ];

in
{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = sysutils ++ desktop ++ editor ++ browser ++ apps ++ utils;
}
