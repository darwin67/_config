{ pkgs, ... }:

{
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];

  networking = {
    networkmanager.enable = true;
    firewall.trustedInterfaces = [ "tailscale0" ];
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

    tailscale = {
      enable = true;
      openFirewall = true;
    };

    openssh = {
      enable = true;
      openFirewall = true;
      ports = [ 22 ];
      settings = {
        PasswordAuthentication = true;
        KbdInteractiveAuthentication = true;
        PermitRootLogin = "no";
        X11Forwarding = false;
      };
      extraConfig = ''
        AllowUsers darwin
      '';
    };
  };

  users.defaultUserShell = pkgs.zsh;
  users.users.darwin = {
    isNormalUser = true;
    description = "Darwin Wu";
    extraGroups = [
      "networkmanager"
      "wheel"
      "docker"
    ];
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

  system.activationScripts.binbash = {
    deps = [ "binsh" ];
    text = ''
      ln -sf /bin/sh /bin/bash
    '';
  };

  system.stateVersion = "26.05";
}
