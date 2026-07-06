{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.local.tuigreet;

  # Sway requires this flag when NVIDIA's proprietary/open driver is present;
  # it is harmless on hosts without NVIDIA.
  swayCommand = "${lib.getExe config.programs.sway.package} --unsupported-gpu";
  tuigreetPackage = pkgs.callPackage (inputs.tuigreet + /nix/package.nix) {
    craneLib = inputs.crane.mkLib pkgs;
  };
  tuigreetConfig = (pkgs.formats.toml { }).generate "tuigreet-config.toml" {
    session.command = swayCommand;

    display = {
      show_time = true;
      time_format = "%a %b %d  %H:%M";
      greeting = "NixOS";
      show_title = true;
      custom_title = cfg.title;
      align_greeting = "center";
    };

    remember = {
      username = true;
      session = true;
    };

    user_menu = {
      enabled = true;
      min_uid = 1000;
      max_uid = 30000;
    };

    secret = {
      mode = "characters";
      characters = "*";
    };

    layout = {
      width = 72;
      container_padding = 2;
      prompt_padding = 1;
    };

    power = {
      shutdown = "${pkgs.systemd}/bin/systemctl poweroff";
      reboot = "${pkgs.systemd}/bin/systemctl reboot";
      suspend = "${pkgs.systemd}/bin/systemctl suspend";
    };

    keybindings = {
      power = 12;
      background = 4;
    };

    # greetd runs tuigreet on a raw Linux VT, where truecolor hex escapes can
    # render as plain white. Use ANSI color names for predictable VT colors.
    theme = {
      border = "blue";
      text = "white";
      prompt = "cyan";
      time = "green";
      action = "cyan";
      button = "lightblue";
      container = "black";
      input = "white";
      greet = "lightcyan";
      title = "lightblue";
    };

    background = {
      kind = "matrix";
      fps = 24;
      matrix = {
        head_color = "lightcyan";
        bright_color = "cyan";
        dim_color = "blue";
        min_length = 6;
        max_length = 18;
        min_speed = 0.30;
        max_speed = 1.10;
        mutate_chance = 0.02;
      };
    };
  };
in
{
  options.local.tuigreet.title = lib.mkOption {
    type = lib.types.str;
    default = "NixOS";
    description = "Title shown by tuigreet.";
  };

  config = {
    environment.etc."tuigreet/config.toml".source = tuigreetConfig;

    # Use a console greeter for Linux hosts and start Sway after authentication.
    # This avoids graphical greeter GPU selection issues on hybrid systems.
    services.greetd = {
      enable = true;
      settings = {
        terminal.vt = 1;
        default_session = {
          user = "greeter";
          command = "${tuigreetPackage}/bin/tuigreet --config /etc/tuigreet/config.toml";
        };
      };
    };

    systemd.tmpfiles.rules = [
      "d /var/cache/tuigreet 0755 greeter greeter -"
    ];
  };
}
