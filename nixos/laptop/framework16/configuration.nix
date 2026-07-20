{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    inputs.nixos-hardware.nixosModules.framework-16-amd-ai-300-series-nvidia
    inputs.lanzaboote.nixosModules.lanzaboote
    ./hardware-configuration.nix
  ];

  local.tuigreet.title = "NixOS - Framework16";

  environment.systemPackages = [ pkgs.sbctl ];

  services.tailscale = {
    enable = true;
    openFirewall = true;
  };
  networking.firewall.trustedInterfaces = [ "tailscale0" ];

  # secure boot - commented out from hardware-configuration.nix
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/var/lib/sbctl";
  };
  boot.resumeDevice = "/dev/mapper/luks-637ad990-8f3d-4854-9e24-1ac18ace2723";
  # ref: https://github.com/NixOS/nixpkgs/issues/489947#issuecomment-3897262330
  # boot.kernelPackages = pkgs.linuxPackages_6_18;

  hardware.nvidia.prime = {
    nvidiaBusId = "PCI:193:0:0"; # c1
    amdgpuBusId = "PCI:194:0:0"; # c2
  };
  hardware.nvidia.primeBatterySaverSpecialisation = true;

  services.pipewire.wireplumber = {
    enable = true;
    extraConfig."51-force-speaker-profile" = {
      "monitor.alsa.rules" = [
        {
          matches = [ { "device.name" = "alsa_card.pci-0000_c2_00.6"; } ];
          actions = {
            update-props = {
              "device.profile" = "HiFi (Mic1, Mic2, Speaker)";
            };
          };
        }
      ];
    };
  };

  sops.secrets.gpg-private-key = {
    sopsFile = "./secrets.enc.yaml";
    owner = "darwin";
    mode = "0400";
  };

  # boot.kernelParams = [ "amdgpu.abmlevel=0" ];

  # NOTE
  # The Framework 16 AMD hardware module enables power-profiles-daemon by
  # default; NixOS rejects configurations with both PPD and TLP enabled.
  services.tlp.enable = lib.mkForce false;

  # systemd.services.dbus-broker.reloadIfChanged = lib.mkForce false;
  # systemd.user.services.dbus-broker.reloadIfChanged = lib.mkForce false;

  # NOTE
  # BIOS: enable "Linux Audio Compatibility" for better speaker output.
}
