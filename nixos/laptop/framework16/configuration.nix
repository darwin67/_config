{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    inputs.nixos-hardware.nixosModules.framework-16-amd-ai-300-series
    ./hardware-configuration.nix
  ];

  hardware.nvidia.prime = {
    amdgpuBusId = "PCI:194:0:0";
    nvidiaBusId = "PCI:193:0:0";
  };

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

  boot.kernelParams = [ "amdgpu.abmlevel=0" ];
  services.tlp.enable = lib.mkForce false;

  # BIOS: enable "Linux Audio Compatibility" for better speaker output.
}
