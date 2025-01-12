{ config, lib, pkgs, ... }:

let
  artwork = pkgs.stdenv.mkDerivation {
    name = "artwork";
    src = pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixos-artwork";
      rev = "63f68a917f4e8586c5d35e050cdaf1309832272d";
      sha256 = "sha256-XquSEijNYtGDkW35bibT2ki18qicENCsIcDzDxrgQkM=";
    };
    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out
      cp $src/wallpapers/*.{png,svg} $out
    '';
  };

in {
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  environment = {
    systemPackages = [ artwork ];
    variables = {
      ARTWORK_PATH = "${artwork}";
      # GLFW_IM_MODULE = "ibus";
    };
  };

  # NOTE:
  # LUKs related settings should be moved to nixos.d/luks.nix
  # and copied to /etc/nixos/luks.nix

  networking.networkmanager.enable = true;

  hardware = {
    bluetooth = {
      enable = true;
      powerOnBoot = true;
      settings = {
        General = {
          Experimental = true;
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };

    # Scanner
    sane = {
      enable = true;
      extraBackends = with pkgs; [
        sane-airscan
        hplipWithPlugin
        epkowa
        utsushi
      ];
      disabledDefaultBackends = [ "escl" ];
    };
  };

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

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    nerdfonts
    font-awesome
  ];

  # Configure keymap in X11
  xdg = { portal = { enable = true; }; };
  services = {
    timesyncd.enable = true;

    # location service
    geoclue2 = {
      enable = true;
      geoProviderUrl = "https://api.beacondb.net/v1/geolocate?key=geoclue";
    };

    # depends on location service
    automatic-timezoned.enable = true;

    fwupd.enable = true;
    # enable the gnome-keyring secrets vault.
    # will be exposed through DBus to programs willing to store secrets
    gnome = { gnome-keyring.enable = true; }; # keyring

    tlp = {
      enable = true;
      settings = {
        CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";

        PLATFORM_PROFILE_ON_AC = "performance";
        PLATFORM_PROFILE_ON_BAT = "balanced";

        # The following prevents the battery from charging fully to
        # preserve lifetime. Run `tlp fullcharge` to temporarily force
        # full charge.
        # https://linrunner.de/tlp/faq/battery.html#how-to-choose-good-battery-charge-thresholds
        # START_CHARGE_THRESH_BAT0 = 40;
        STOP_CHARGE_THRESH_BAT0 = 90;

        # 100 being the maximum, limit the speed of my CPU to reduce
        # heat and increase battery usage:
        # CPU_MAX_PERF_ON_AC = 100;
        # CPU_MAX_PERF_ON_BAT = 90;
      };
    };
    libinput.enable = true;
    displayManager = {
      enable = true;
      defaultSession = "sway";
      sddm = {
        enable = true;
        wayland.enable = true;
      };
    };
    xserver = {
      enable = true;
      desktopManager = { runXdgAutostartIfNone = true; };
      xkb = {
        layout = "us";
        variant = "";
        options = "ctrl:swapcaps"; # remap caps lock to control
      };
    };

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
      package = pkgs.emacs;
    };

    # printing
    printing = {
      enable = true;
      drivers = with pkgs; [ hplip ];
    };
    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
    # scanner
    udev = { packages = with pkgs; [ sane-airscan utsushi ]; };

    flatpak.enable = true;
    dbus.enable = true;
    colord.enable = true;
  };

  users.defaultUserShell = pkgs.zsh;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.darwin = {
    isNormalUser = true;
    description = "Darwin Wu";
    extraGroups = [ "networkmanager" "wheel" "docker" "scanner" "lp" ];
    useDefaultShell = true;
  };

  # Allow unfree packages
  # nixpkgs.config = {
  #   allowUnfree = true;
  #   allowBroken = false;
  # };

  systemd = {
    # configuring sway itself
    user = {
      targets.sway-session = {
        description = "sway compositor session";
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

        # for controller via bluetooth devices
        mpris-proxy = {
          description = "Mpris proxy";
          after = [ "network.target" "sound.target" ];
          wantedBy = [ "default.target" ];
          serviceConfig.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
        };
      };
    };
  };

  programs = {
    ssh.startAgent = true;
    zsh.enable = true;
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
        sway-contrib.grimshot
        mako # notification
        # swaynotificationcenter
        kanshi
        grim
        slurp
        wl-clipboard
        wf-recorder
        (python311.withPackages (ps: with ps; [ i3pystatus keyring ]))
        satty

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
    nix-ld.enable = true;

    _1password.enable = true;
    _1password-gui = {
      enable = true;
      # Certain features, including CLI integration and system authentication support,
      # require enabling PolKit integration on some desktop environments (e.g. Plasma).
      polkitPolicyOwners = [ "darwin" ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11";

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

  # link /bin/sh to /bin/bash because it doesn't exist and it's a fucking pain
  system.activationScripts.binbash = {
    deps = [ "binsh" ];
    text = ''
      ln -sf /bin/sh /bin/bash
    '';
  };

  ## Security
  security = {
    rtkit.enable = true;
    pam.services.sddm.enableGnomeKeyring = true;
  };

}
