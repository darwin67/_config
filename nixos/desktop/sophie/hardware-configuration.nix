# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices."luks-f9a7e3fb-c7e9-45d0-bc5e-b2258aefc5de".device = "/dev/disk/by-uuid/f9a7e3fb-c7e9-45d0-bc5e-b2258aefc5de";
  networking.hostId = "4e98920d";
  networking.hostName = "sophie"; # Define your hostname.

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/14b951b4-bd5c-454d-b6fb-162741eb91ef";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."luks-dcc35be9-edc4-4589-8dae-84d62df0012a".device = "/dev/disk/by-uuid/dcc35be9-edc4-4589-8dae-84d62df0012a";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/0A56-F377";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/d91b2aff-8443-434e-b013-f7ed75ef9785"; }
    ];

  boot.supportedFilesystems = ["zfs"];
  boot.zfs = {
    forceImportRoot = false;
    extraPools = [ "storage" ];
  };

  fileSystems."/home/darwin/storage" = {
      device = "/mnt/storage";
      fsType = "none";
      options = [ "bind" ];
  };


  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.eno1.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp9s0f3u2u1.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
