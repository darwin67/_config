{ config, pkgs, ... }:

let vantaCreds = import ./vanta/credentials.nix;
in {
  imports = [ ./vanta/module.nix ];

  services.vanta = {
    enable = vantaCreds.ENABLE;
    agentKey = vantaCreds.VANTA_KEY;
    email = vantaCreds.VANTA_OWNER_EMAIL;
  };
}
