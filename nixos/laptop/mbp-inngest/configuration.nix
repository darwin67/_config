{ config, lib, pkgs, ... }:

{
  imports = [ ../../common/apple/conf.nix ../../common/apple/pkg.nix ];

  homebrew = {
    casks = [ "slack" "zoom" "notion" "linear-linear" "tailscale" ];
  };
}
