{ config, lib, pkgs, ... }:

let
  editor = with pkgs; [
    vim
    neovim
    emacs30 # 29
    vscode

    # editor utilities
    nixfmt-classic
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
    nodePackages.vscode-json-languageserver

    # Other
    claude-code
  ];

  apps = with pkgs; [
    signal-desktop
    whatsapp-for-linux
    # wechat-uos
    discord
    nordic
    _1password-gui
    _1password-cli # cli tool
    insync
    mpv
    yt-dlp
    spotify
    insync
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
    xfce.thunar
    file
    seahorse
    kitty
    yazi
    flatpak
    zip
    unzip
    ghostty
    gcr_4
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

  browser = with pkgs; [ firefox-devedition google-chrome brave zen-browser ];

  utils = with pkgs; [
    neofetch
    i3pystatus
    (python311.withPackages (ps:
      with ps; [
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
      ]))

    docker
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
    bottom
    dig
    sops
    age
    duckdb

    # Terminal AI tools
    claude-code
    gemini-cli
    codex

    # scanner
    simple-scan
  ];

in {
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = sysutils ++ desktop ++ editor ++ browser ++ apps
    ++ utils;
}
