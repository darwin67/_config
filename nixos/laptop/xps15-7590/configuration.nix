{ config, lib, pkgs, ... }:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    ./hardware-configuration.nix
  ];

  environment.systemPackages = with pkgs; [ slack ];
}
