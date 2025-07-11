{ config, lib, pkgs, self, username, ... }:

{
  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";
  nix.enable = false;

  system = {
    primaryUser = username;

    # Set Git commit hash for darwin-version.
    configurationRevision = self.rev or self.dirtyRev or null;

    # Used for backwards compatibility, please read the changelog before changing.
    # $ darwin-rebuild changelog
    stateVersion = 5;

    defaults = {
      dock = {
        autohide = true;
        magnification = true;
      };

      spaces.spans-displays = true;

      NSGlobalDomain = {
        # Shortest value based on
        # defaults read NSGlobalDomain InitialKeyRepeat
        InitialKeyRepeat = 15;
        KeyRepeat = 2;

        "com.apple.mouse.tapBehavior" = 1; # Enable tap to click
        "com.apple.trackpad.scaling" = 2.0;
      };

      controlcenter = { BatteryShowPercentage = true; };

      loginwindow = { GuestEnabled = false; };

      trackpad.Clicking = true;
    };

    # Remaps
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";

  users.users."${username}" = {
    name = username;
    home = "/Users/${username}";
  };

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-cjk-sans
    nerd-fonts._0xproto
    nerd-fonts.droid-sans-mono
    font-awesome
  ];

  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "uninstall";
    };

    taps = [ ];
    brews = [ "watch" ];
    casks = [
      # Terminal
      "ghostty"
      "kitty"
      "alacritty"

      # Browwer
      "google-chrome"
      "firefox@developer-edition"
      "zen"
      "brave-browser"

      # Chat
      "whatsapp"
      "signal"
      "discord"

      # Tool
      "1password"
      "1password-cli"
      "obsidian"
      "google-drive"
      "copyq"
      "docker-desktop"
      "nikitabobko/tap/aerospace" # tiling window manager
      "wireshark-app"
      "alfred"
      "yubico-yubikey-manager"

      # Entertainment
      "spotify"

      # Development
      "emacs-app"
      "zed"
      "visual-studio-code"
      "android-studio"
    ];

    masApps = { };
  };

  programs = { zsh.enable = true; };

  services = { emacs.enable = true; };
  # services.nix-daemon.enable = true;
}
