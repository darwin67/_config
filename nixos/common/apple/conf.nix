{ config, lib, pkgs, self, username, ... }:

{
  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Set Git commit hash for darwin-version.
  system.configurationRevision = self.rev or self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

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

    taps = [];
    brews = [
    ];
    casks = [
      # Terminal
      "ghostty"
      "kitty"
      "alacritty"

      # Browwer
      "google-chrome"
      "firefox@developer-edition"
      "zen-browser"

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
      "docker"

      # Entertainment
      "spotify"

      # Development
      "emacs"
      "zed"
      "visual-studio-code"
      "android-studio"
    ];

    masApps = {};
  };

  programs.zsh.enable = true;

  services = {
    nix-daemon.enable = true;
  };
}
