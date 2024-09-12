{ config, lib, pkgs, ... }:

let
  # wallutils helper script to convert .heic format wallpapers
  heicinstall = pkgs.stdenv.mkDerivation {
    name = "heic-install";
    src = builtins.fetchurl {
      url =
        "https://raw.githubusercontent.com/xyproto/wallutils/main/scripts/heic-install";
    };
    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/heic-install
      sed -i -e 's/\r$//' $out/bin/heic-install
      chmod +x $out/bin/heic-install
    '';
  };

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
    _1password # cli tool
    insync
    mpv
    yt-dlp
    spotify

    # Work
    slack
    zoom-us
    redisinsight
    insomnia
  ];

  sysutils = with pkgs; [
    zsh
    pulseaudio
    gnumake
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
    gnome.gnome-terminal
    kitty
    yazi
  ];

  desktop = with pkgs; [
    sway
    rtkit # for pipewire
    polkit # for 1password
    polkit_gnome
    playerctl
    brightnessctl
    bluez
    bluez-tools

    fwupd # hardware
    tlp # battery management
    inotify-tools
    graphviz
    libxml2
    libayatana-appindicator
  ];

  browser = with pkgs; [
    firefox-bin
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
        nose
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
  ];

in {
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = sysutils ++ desktop ++ editor ++ browser ++ apps
    ++ utils ++ [ heicinstall ];

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
