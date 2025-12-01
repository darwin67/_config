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
    # REF: https://github.com/NixOS/nixpkgs/issues/395169
    # probably can be disabled when fixed
    (emacs30.override { withNativeCompilation = false; })

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
    xh
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
    cocoapods
    duckdb

    # Terminal AI tools
    claude-code
    gemini-cli
    codex

    # go pprof visulization dep
    graphviz
  ];

in { environment.systemPackages = editor ++ utils ++ sysutils ++ dev; }
