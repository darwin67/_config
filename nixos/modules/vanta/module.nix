{ config, lib, pkgs, ... }:

with lib;

let cfg = config.services.vanta;
in {
  options.services.vanta = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Vanta agent service";
    };

    configFile = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable (let vanta = pkgs.callPackage ./default.nix { };

  in {
    environment = {
      systemPackages = [ vanta ];

      etc."vanta.conf" = {
        source = cfg.configFile;
        mode = "0600";
      };
    };

    systemd.services.vanta = {
      after = [ "network.service" "syslog.service" ];
      description = "Vanta agent";
      wantedBy = [ "multi-user.target" ];
      script = ''
        /var/vanta/metalauncher
      '';

      serviceConfig = {
        TimeoutStartSec = 0;
        Restart = "on-failure";
        KillMode = "control-group";
        KillSignal = "SIGTERM";
      };
    };
  });
}
