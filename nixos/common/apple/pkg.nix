{ config, lib, pkgs, ... }:

# List packages installed in system profile. To search by name, run:
# $ nix-env -qaP | grep wget
let
  editor = with pkgs; [
    vim
    neovim
    emacs # 29
    vscode

    nixfmt
    editorconfig-core-c
    shfmt
    shellcheck
    glslang
    markdownlint-cli

    # LSPs
    vimPlugins.vim-lsp
    nixd
    nodePackages.vscode-json-languageserver
  ];

  apps = with pkgs; [
    google-chrome
    signal-desktop
    whatsapp-for-mac

    discord
    # nordic
    _1password-gui
    _1password-cli # cli tool

    yt-dlp
    spotify
    # obsidian
  ];

  utils = with pkgs; [
    neofetch
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
  ];

  sysutils = with pkgs; [
    zsh
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

    # file
    kitty
    yazi
    zip
    unzip
    ghostty
  ];

in { environment.systemPackages = sysutils ++ editor ++ utils + apps; }
