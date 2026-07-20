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

  networking = {
    networkmanager.enable = lib.mkForce false;
    useDHCP = lib.mkForce false;
  };

  services.resolved.enable = true;

  systemd.network = {
    enable = true;
    wait-online.anyInterface = true;

    netdevs."20-br-virt0" = {
      netdevConfig = {
        Kind = "bridge";
        Name = "br-virt0";
        MACAddress = "b4:2e:99:ed:15:7b";
      };
    };

    networks."10-eno1" = {
      matchConfig.Name = "eno1";
      networkConfig.Bridge = "br-virt0";
      linkConfig.RequiredForOnline = "enslaved";
    };

    networks."20-br-virt0" = {
      matchConfig.Name = "br-virt0";
      networkConfig = {
        DHCP = "yes";
        IPv6AcceptRA = true;
      };
      linkConfig.RequiredForOnline = "routable";
    };
  };

  virtualisation.libvirtd.enable = true;
  users.users.darwin.extraGroups = lib.mkAfter [
    "kvm"
    "libvirtd"
  ];

  # tailscale
  services.tailscale = {
    useRoutingFeatures = "server";
  };
  networking.firewall.trustedInterfaces = [ "tailscale0" ];
}
