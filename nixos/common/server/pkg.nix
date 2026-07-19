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

  editors = with pkgs; [
    vim
    neovim
    emacs
    nixfmt
    editorconfig-core-c
    shfmt
    shellcheck
    markdownlint-cli
    tree-sitter
    nixd
    vscode-json-languageserver
  ];

  sysutils = with pkgs; [
    zsh
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
    glibcLocales
    file
    yazi
    zip
    unzip
  ];

  devutils = with pkgs; [
    fastfetch
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
    bottom
    dig
    sops
    age
    duckdb
    uv
    gh
    tpm2-tools

    (python313.withPackages (
      ps: with ps; [
        pip
        pytest
        pyflakes
        isort
        cffi
        ipython
        black
      ]
    ))

    latestPkgs.claude-code
    latestPkgs.opencode

    inputs.codex-cli-nix.packages.${pkgs.stdenv.hostPlatform.system}.default
    bubblewrap
  ];
in
{
  environment.systemPackages = sysutils ++ editors ++ devutils;
}
