{ config, lib, pkgs, ... }:

{
  security = {
    polkit.enable = true;
    rtkit.enable = true;
    pam.services = { sddm.enableGnomeKeyring = true; };
  };
}
