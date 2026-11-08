# Tools shared by every Linux host. Imported from mkLinuxSystem in
# flake.nix, so both the desktop and server profiles get them; keep
# profile- or host-specific packages in the profile pkg.nix files.
{
  inputs,
  pkgs,
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
    jujutsu
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
    bubblewrap # dep for codex

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
  ];
in
{
  imports = [ ../llm-agents.nix ];

  environment.systemPackages = editors ++ sysutils ++ devutils;
}
