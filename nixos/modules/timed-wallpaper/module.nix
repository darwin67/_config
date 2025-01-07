{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.timed-wallpaper;
  timed-wallpaper = (pkgs.callPackage ./default.nix { inherit cfg; });

in {
  options.services.timed-wallpaper = {
    enable = mkOption {
      type = types.bool;
      default = true;
      description = "Time base wallpaper";
    };

    theme = mkOption { type = types.str; };
  };

  config = mkIf cfg.enable {
    environment = { systemPackages = [ timed-wallpaper ]; };

    # NOTE: need to create a directory to /usr/share/backgrounds
    # because `settimed` only reads from that path
    system.activationScripts.backgroundSymlink = {
      text = ''
        rm -rf /usr/share/backgrounds/${cfg.theme}
        cp -R ${timed-wallpaper}/* /usr/share/backgrounds/
      '';
      deps = [ ];
    };

    systemd.user.services.timed-wallpaper = {
      description = "Time based wallpaper";
      wantedBy = [ "sway-session.target" ];
      after = [ "sway-session.target" ];
      path = [ pkgs.sway ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.wallutils}/bin/settimed ${cfg.theme}";
        ExecStop = "${pkgs.procps}/bin/pkill settimed";
      };
    };
  };
}
