{
  config,
  lib,
  ...
}:

{
  networking = {
    networkmanager.enable = true;
    firewall.trustedInterfaces = [ "tailscale0" ];
  };

  services = {
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

  users.users.darwin = {
    extraGroups = [
      "wheel"
      "docker"
    ]
    ++ lib.optional config.networking.networkmanager.enable "networkmanager";
  };
}
