# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  wallpaperTheme = "macMonterey";
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

in {
  imports = [ # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices."luks-6f2cda4d-854b-4096-93ee-a0bdeadd03e3".device =
    "/dev/disk/by-uuid/6f2cda4d-854b-4096-93ee-a0bdeadd03e3";
  networking.hostName = "nixos-xps15-9550"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
    };
  };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };

    inputMethod = {
      enabled = "fcitx5";
      fcitx5 = {
        addons = [
          pkgs.fcitx5-mozc
          pkgs.fcitx5-rime
          pkgs.fcitx5-gtk
          pkgs.fcitx5-configtool
        ];
      };
    };
  };
  # environment.variables.GLFW_IM_MODULE = "ibus";

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    nerdfonts
    font-awesome
  ];

  # Configure keymap in X11
  services = {
    tlp.enable = true;
    xserver = {
      enable = true;
      displayManager = {
        defaultSession = "sway";
        gdm.enable = true;
        # sddm.enable = true;
      };
      desktopManager = { runXdgAutostartIfNone = true; };
      libinput.enable = true;
      layout = "us";
      xkbVariant = "";
      xkbOptions = "ctrl:swapcaps"; # remap caps lock to control
    };
    # enable the gnome-keyring secrets vault.
    # will be exposed through DBus to programs willing to store secrets
    gnome.gnome-keyring.enable = true;

    # Audio
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };
    blueman.enable = true;

    emacs = {
      enable = true;
      package = pkgs.emacs29;
    };
  };

  virtualisation = { docker.enable = true; };

  security = {
    polkit.enable = true;
    rtkit.enable = true;
  };

  users.defaultUserShell = pkgs.zsh;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.darwin = {
    isNormalUser = true;
    description = "Darwin Wu";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = [ heicinstall ];
    useDefaultShell = true;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    zsh
    pulseaudio
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    neovim
    tmux
    wget
    curlie
    emacs29
    git
    hub
    sway
    fzf
    alacritty

    firefox-bin
    firefox-devedition-bin
    google-chrome

    nordic
    _1password-gui
    _1password # cli tool
    insync
    mpv
    youtube-dl
    spotify
    gnumake

    rtkit # for pipewire
    polkit # for 1password
    polkit_gnome

    gnome.nautilus

    ## Comms
    signal-desktop
    whatsapp-for-linux

    playerctl
    brightnessctl

    i3pystatus
    (python311.withPackages (ps: with ps; [ i3pystatus keyring ]))

    libayatana-appindicator

    docker
    android-studio
    bottom

    fwupd # hardware
    tlp # battery management
    inotify-tools

    # Lang
    flutter
    jdk
  ];

  systemd = {
    # configuring sway itself
    user = {
      targets.sway-session = {
        description = "Sway compositor session";
        documentation = [ "man:systemd.special(7)" ];
        bindsTo = [ "graphical-session.target" ];
        wants = [ "graphical-session-pre.target" ];
        after = [ "graphical-session-pre.target" ];
      };
      services = {
        # configuring kanshi
        kanshi = {
          description = "Kanshi output autoconfig";
          wantedBy = [ "graphical-session.target" ];
          partOf = [ "graphical-session.target" ];
          environment = { XDG_CONFIG_HOME = "/home/darwin/.config"; };
          serviceConfig = {
            ExecStart = ''
              ${pkgs.kanshi}/bin/kanshi
            '';
            RestartSec = 5;
            Restart = "always";
          };
        };

        # polkit
        polkit-gnome-authentication-agent-1 = {
          description = "polkit-gnome-authentication-agent-1";
          wantedBy = [ "graphical-session.target" ];
          wants = [ "graphical-session.target" ];
          after = [ "graphical-session.target" ];
          serviceConfig = {
            Type = "simple";
            ExecStart =
              "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
            Restart = "on-failure";
            RestartSec = 1;
            TimeoutStopSec = 10;
          };
        };

        dynamic-wallpaper = {
          description = "Dynamic Wallpaper";
          wantedBy = [ "sway-session.target" ];
          # wants = [ "sway-session.target" ];
          after = [ "sway-session.target" ];
          path = [ pkgs.sway ];
          serviceConfig = {
            Type = "simple";
            ExecStart = ''${pkgs.wallutils}/bin/settimed "${wallpaperTheme}"'';
            ExecStop = "${pkgs.procps}/bin/pkill settimed";
          };
        };
      };
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  programs = {
    zsh = { enable = true; };
    ssh.startAgent = true;
    sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      extraPackages = with pkgs; [
        wofi
        wob
        swaylock-effects
        swayidle
        xwayland
        waybar
        # mako
        swaynotificationcenter
        kanshi
        grim
        slurp
        wl-clipboard
        wf-recorder
        (python311.withPackages (ps: with ps; [ i3pystatus keyring ]))

        # wallpaper
        wallutils
        imagemagick
        libheif
      ];
      extraSessionCommands = ''
        export XDG_CURRENT_DESKTOP=sway
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export XDG_SESSION_TYPE=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export MOZ_ENABLE_WAYLAND=1
      '';
    };
    waybar = { enable = false; };

    _1password.enable = true;
    _1password-gui = {
      enable = true;
      # Certain features, including CLI integration and system authentication support,
      # require enabling PolKit integration on some desktop environments (e.g. Plasma).
      polkitPolicyOwners = [ "darwin" ];
    };
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

  # link /bin/sh to /bin/bash because it doesn't exist and it's a fucking pain
  system.activationScripts.binbash = {
    deps = [ "binsh" ];
    text = ''
      ln -sf /bin/sh /bin/bash
    '';
  };

}
