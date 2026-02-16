{ pkgs, self, lib, ... }:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    ./hardware-configuration.nix
  ];

  # ZFS support
  boot.supportedFilesystems = [ "zfs" ];
  boot.zfs = {
    forceImportRoot = false;
    extraPools = [ "storage" ];
  };

  fileSystems."/home/darwin/storage" = {
    device = "/mnt/storage";
    fsType = "none";
    options = [ "bind" ];
  };

  virtualisation.libvirtd.enable = true;
  programs.virt-manager.enable = true;
  users.users.darwin.extraGroups = lib.mkAfter [ "kvm" "libvirtd" ];
}
