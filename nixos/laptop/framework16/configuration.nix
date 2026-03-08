{ config, lib, pkgs, ... }:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    ./hardware-configuration.nix
  ];

  services.pipewire.wireplumber = {
    enable = true;
    extraConfig."51-force-speaker-profile" = {
      "monitor.alsa.rules" = [{
        matches = [{ "device.name" = "alsa_card.pci-0000_c2_00.6"; }];
        actions = {
          update-props = { "device.profile" = "HiFi (Mic1, Mic2, Speaker)"; };
        };
      }];
    };
  };
}
