{ config, lib, pkgs, ... }:

# These are generally terminal related packages.
# GUI apps are installed via homebrew in conf.nix
#
# List packages installed in system profile. To search by name, run:
# $ nix-env -qaP | grep wget
let
  editor = with pkgs; [
    vim
    neovim

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
    yt-dlp

    # darwin.xcode
    xcode-install
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

    # file
    yazi
    zip
    unzip
  ];

  dev = with pkgs; [
    flutter
  ];

in { environment.systemPackages = editor ++ utils ++ sysutils ++ dev; }
