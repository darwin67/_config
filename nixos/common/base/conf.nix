# Base settings shared by every Linux host. Imported from mkLinuxSystem in
# flake.nix, so both the desktop and server profiles get them; keep
# profile- or host-specific settings in the profile and host configs.
{
  pkgs,
  ...
}:

{
  nix = {
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];

    # Daily garbage collection via the nix-gc systemd timer.
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };
  };

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
  };

  services = {
    timesyncd.enable = true;
    fwupd.enable = true;
    dbus.enable = true;

    avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };

  users.defaultUserShell = pkgs.zsh;
  users.users.darwin = {
    isNormalUser = true;
    description = "Darwin Wu";
    useDefaultShell = true;
  };

  programs = {
    ssh.startAgent = true;
    zsh.enable = true;
    nix-ld.enable = true;
  };

  virtualisation.docker = {
    enable = true;
    package = pkgs.docker_29;
  };

  system = {
    # link /bin/sh to /bin/bash because it doesn't exist and it's a fucking pain
    activationScripts.binbash = {
      deps = [ "binsh" ];
      text = ''
        ln -sf /bin/sh /bin/bash
      '';
    };

    stateVersion = "26.05";
  };
}
