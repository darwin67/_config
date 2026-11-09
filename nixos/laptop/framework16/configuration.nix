{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  imports = [
    ../../common/desktop/conf.nix
    ../../common/desktop/pkg.nix
    inputs.nixos-hardware.nixosModules.framework-16-amd-ai-300-series-nvidia
    inputs.lanzaboote.nixosModules.lanzaboote
    ./hardware-configuration.nix
  ];

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

  # The niri greeter uses the same monitor arrangement as the desktop
  # (niri/host.kdl), with the login window on the 32" in the middle.
  #
  # Every display is wired to the AMD iGPU (c2), so the greeter renders there.
  # Rendering on the NVIDIA dGPU would PRIME-copy each frame into the amdgpu,
  # whose 512MB VRAM carveout was too small with the MST dock connected at
  # boot: "Failed to pin framebuffer with error -12" and a greetd restart loop.
  # niri ignores wlroots' WLR_DRM_DEVICES; renderDevice is its equivalent.
  programs.genkan.greeter = {
    authenticationOutput = "Dell Inc. DELL U3223QE 5PRFH34";
    niri = {
      renderDevice = "/dev/dri/by-path/pci-0000:c2:00.0-render";
      outputs = {
        "Dell Inc. DELL S2421HGF DWH4B73" = {
          scale = 1.0;
          position = {
            x = 0;
            y = 614;
          };
        };
        "Dell Inc. DELL U3223QE 5PRFH34" = {
          scale = 1.275;
          position = {
            x = 1920;
            y = 0;
          };
        };
        "Dell Inc. DELL U2720QM 604QZ83" = {
          transform = "90";
          scale = 1.3;
          position = {
            x = 4932;
            y = 0;
          };
        };
        "eDP-1" = {
          scale = 1.2;
          position = {
            x = 0;
            y = 1694;
          };
        };
      };
    };
  };

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

  sops.age.keyFile = "${config.users.users.darwin.home}/.config/sops/age/keys.txt";

  sops.secrets =
    lib.genAttrs
      [
        "gpg-private-key"
        # API keys read by the ai-usagebar wrapper in nixos/common/desktop/pkg.nix.
        "ollama-api-key"
        "deepseek-api-key"
        "opencode-go-api-key" # OpenCode service account key with Usage: Read only
      ]
      (_: {
        sopsFile = ./secrets.enc.yaml;
        owner = "darwin";
        mode = "0400";
      });

  # boot.kernelParams = [ "amdgpu.abmlevel=0" ];

  # NOTE
  # The Framework 16 AMD hardware module enables power-profiles-daemon by
  # default; NixOS rejects configurations with both PPD and TLP enabled.
  services.tlp.enable = lib.mkForce false;

  services.davfs2.enable = true;

  # systemd.services.dbus-broker.reloadIfChanged = lib.mkForce false;
  # systemd.user.services.dbus-broker.reloadIfChanged = lib.mkForce false;

  # NOTE
  # BIOS: enable "Linux Audio Compatibility" for better speaker output.
}
