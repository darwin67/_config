{ config, lib, pkgs, ... }:

let
  editor = with pkgs; [
    vim
    neovim
    emacs29
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

    # Work
    slack
    zoom-us
    insomnia
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
    tailscale
    sops

    # scanner
    simple-scan
  ];

in {
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = sysutils ++ desktop ++ editor ++ browser ++ apps
    ++ utils;

  programs = {
    zsh.enable = true;

    _1password.enable = true;
    _1password-gui = {
      enable = true;
      # Certain features, including CLI integration and system authentication support,
      # require enabling PolKit integration on some desktop environments (e.g. Plasma).
      polkitPolicyOwners = [ "darwin" ];
    };
  };

  virtualisation = {
    # podman = {
    #   enable = true;
    #   dockerCompat = true;
    #   defaultNetwork.settings.dns_enabled = true;

    #   dockerSocket.enable = true;
    #   autoPrune.enable = true;
    # };

    docker = {
      enable = true;

      # NOTE: doesn't work with host.docker.internal
      # rootless = {
      #   enable = true;
      #   setSocketVariable = true;
      # };
      # autoPrune.enable = true;
    };
  };
}
