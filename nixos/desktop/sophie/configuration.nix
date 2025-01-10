{ pkgs, self, ... }:

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
}
