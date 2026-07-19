{
  lib,
  ...
}:

{
  imports = [ ./hardware-configuration.nix ];

  boot.initrd = {
    systemd.enable = true;
    luks.devices = {
      "luks-f9a7e3fb-c7e9-45d0-bc5e-b2258aefc5de".crypttabExtraOpts = [
        "tpm2-device=auto"
      ];
      "luks-dcc35be9-edc4-4589-8dae-84d62df0012a".crypttabExtraOpts = [
        "tpm2-device=auto"
      ];
    };
  };

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

  security.tpm2.enable = true;

  virtualisation.libvirtd.enable = true;
  users.users.darwin.extraGroups = lib.mkAfter [
    "kvm"
    "libvirtd"
  ];

  # tailscale
  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "server";
  };
  networking.firewall.trustedInterfaces = [ "tailscale0" ];
}
