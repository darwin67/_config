{ config, lib, pkgs, ... }:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    ./hardware-configuration.nix

    # Work related
    ../../modules/vanta/module.nix
  ];

  environment.systemPackages = with pkgs; [
    slack
    zoom-us
    insomnia

    tailscale
  ];

  services = {
    tailscale = { enable = true; };

    # TODO encrypt these credentials
    # vanta = {
    #   enable = true;
    #   agentKey = "";
    #   email = "";
    # };
  };

  # Docker related tweaks
  networking = {
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
}
