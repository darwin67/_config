{ pkgs, self, ... }:

{
  imports = [ ../../common/apple/conf.nix ../../common/apple/pkg.nix ];

  homebrew = {
    casks = [
      "slack"
      "zoom"
      "linear"
      "tailscale-app"
    ];
  };
}
