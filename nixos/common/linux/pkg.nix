{ config, lib, pkgs, ... }:

let
  editor = with pkgs; [
    vim
    neovim
    emacs # 29
    vscode

    # editor utilities
    nixfmt
    editorconfig-core-c
    shfmt
    shellcheck
    glslang
    markdownlint-cli

    # org mode
    maim
    scrot
    gnuplot

    # LSPs
    vimPlugins.vim-lsp
    nixd
    nodePackages.vscode-json-languageserver
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
    tmux
    wget
    curlie
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
    # firefox-bin
    firefox-devedition-bin
    google-chrome
    brave
    zen-browser
  ];

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

    # scanner
    simple-scan
  ];

in {
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = sysutils ++ desktop ++ editor ++ browser ++ apps
    ++ utils;
}
