{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

let
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
    kubectl
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
    dig
    sops
    age
    duckdb
    uv
    gh
    tpm2-tools
    jujutsu

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

    bubblewrap
  ];
in
{
  imports = [ ../llm-agents.nix ];

  environment.systemPackages = sysutils ++ editors ++ devutils;
}
