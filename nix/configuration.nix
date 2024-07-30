# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let wallpaperTheme = "macMonterey";

in {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./nixos.d/luks.nix
    ./nixos.d/packages.nix
    ./nixos.d/security.nix
    ./modules/inngest.nix
    <home-manager/nixos>
  ];

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # Bootloader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

  # NOTE:
  # LUKs related settings should be moved to nixos.d/luks.nix
  # and copied to /etc/nixos/luks.nix

  # Enable networking
  networking = {
    networkmanager.enable = true;

    firewall = {
      # No way to apply the interface configuration to all interfaces
      # generated by docker-compose
      enable = true;

      # required to make sure host.docker.internal works
      interfaces = {
        "docker0" = {
          allowedTCPPortRanges = [{
            from = 3000;
            to = 65535;
          }];
        };
        # NOTE: the local bridge interface name
        # `ip link show`
        "br-676bcf9c1def" = {
          allowedTCPPortRanges = [{
            from = 3000;
            to = 65535;
          }];
        };
      };
    };
  };

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
  # environment.variables.GLFW_IM_MODULE = "ibus";

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    nerdfonts
    font-awesome
  ];

  # Configure keymap in X11
  services = {
    timesyncd.enable = true;
    geoclue2.enable = true;
    automatic-timezoned.enable = true;
    fwupd.enable = true;
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
      package = pkgs.emacs29;
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
  };

  users.defaultUserShell = pkgs.zsh;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.darwin = {
    isNormalUser = true;
    description = "Darwin Wu";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    useDefaultShell = true;
  };
  home-manager.useGlobalPkgs = true;
  home-manager.users.darwin = { pkgs, ... }: {
    home.pointerCursor = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
      size = 24;
      x11 = {
        enable = true;
        defaultCursor = "Adwaita";
      };
    };

    home.packages = with pkgs; [ starship ];

    home.file = {
      ".alacritty.toml" = { source = "/home/darwin/_config/.alacritty.toml"; };
      ".gitconfig" = { source = "/home/darwin/_config/.gitconfig"; };
      ".gitignore_global" = {
        source = "/home/darwin/_config/.gitignore_global";
      };
      ".pryrc" = { source = "/home/darwin/_config/.pryrc"; };
      ".npmrc" = { source = "/home/darwin/_config/.npmrc"; };
      ".tmux.conf" = { source = "/home/darwin/_config/.tmux.conf"; };
      ".zlogin" = { source = "/home/darwin/_config/.zlogin"; };
      ".zsh_plugins.txt" = {
        source = "/home/darwin/_config/.zsh_plugins.txt";
      };
      ".zshrc" = { source = "/home/darwin/_config/.zshrc"; };

      ".config/nvim/init.vim" = { source = "/home/darwin/_config/init.vim"; };
      ".config/nvim/lua" = { source = "/home/darwin/_config/nvim"; };
      # Directory
      ".doom.d" = { source = "/home/darwin/_config/.doom.d"; };
      ".config/zsh/functions" = { source = "/home/darwin/_config/zfunc"; };

      # Sway
      ".config/sway" = { source = "/home/darwin/_config/sway"; };
      ".config/starship.toml" = {
        source = "/home/darwin/_config/starship.toml";
      };
      ".config/waybar/config" = {
        source = "/home/darwin/_config/waybar/config.json";
      };
      ".config/waybar/style.css" = {
        source = "/home/darwin/_config/waybar/style.css";
      };
      ".config/wofi" = { source = "/home/darwin/_config/wofi"; };
      ".config/wob" = { source = "/home/darwin/_config/wob"; };

      # Flags
      ".config/brave-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland
        '';
      };
      ".config/chrome-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland
        '';
      };
      ".config/discord-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland
        '';
      };
      ".config/zoom-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland
        '';
      };
    };

    home.sessionVariables = {
      EDITOR = "vim";
      SSH_KEY_PATH = "~/.ssh/";

      # Python
      PY_COLORS = "1";

      # Wayland
      MOZ_ENABLE_WAYLAND = 1;
    };

    programs = {
      zsh = {
        enable = true;
        enableCompletion = true;
        syntaxHighlighting.enable = true;
        history.size = 10000;
      };
      direnv = {
        enable = true;
        nix-direnv = { enable = true; };
      };
    };

    home.stateVersion = "24.05";
  };

  # Allow unfree packages
  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = false;
  };

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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  programs = {
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
        sway-contrib.grimshot
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
    nix-ld.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05";

  # link /bin/sh to /bin/bash because it doesn't exist and it's a fucking pain
  system.activationScripts.binbash = {
    deps = [ "binsh" ];
    text = ''
      ln -sf /bin/sh /bin/bash
    '';
  };
}
