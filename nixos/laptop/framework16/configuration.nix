{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    # inputs.nixos-hardware.nixosModules.framework-16-amd-ai-300-series-nvidia
    inputs.lanzaboote.nixosModules.lanzaboote
    ./hardware-configuration.nix
  ];

  environment.systemPackages = [ pkgs.sbctl ];

  # secure boot - commented out from hardware-configuration.nix
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/var/lib/sbctl";
  };
  # ref: https://github.com/NixOS/nixpkgs/issues/489947#issuecomment-3897262330
  # boot.kernelPackages = pkgs.linuxPackages_6_18;

  hardware.nvidia.prime = {
    nvidiaBusId = "PCI:193:0:0"; # c1
    amdgpuBusId = "PCI:194:0:0"; # c2
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

  # boot.kernelParams = [ "amdgpu.abmlevel=0" ];
  services.tlp.enable = lib.mkForce false;

  # BIOS: enable "Linux Audio Compatibility" for better speaker output.
}
