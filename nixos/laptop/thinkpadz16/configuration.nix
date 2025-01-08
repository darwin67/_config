{ config, lib, pkgs, ... }:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    ./hardware-configuration.nix

    # Work related
    ../../modules/vanta/module.nix
  ];

  # TODO encrypt these credentials
  # services.vanta = {
  #   enable = true;
  #   agentKey = "";
  #   email = "";
  # };
}
