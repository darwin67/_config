{ config, lib, pkgs, ... }:

{
  imports = [
    ../../common/linux/conf.nix
    ../../common/linux/pkg.nix
    ./hardware-configuration.nix
  ];
}
