{ config, lib, pkgs, ... }:

{
  security = {
    polkit.enable = true;
    rtkit.enable = true;
    pam.services.sddm.enableGnomeKeyring = true;
  };

  # enable the gnome-keyring secrets vault.
  # will be exposed through DBus to programs willing to store secrets
  services.gnome.gnome-keyring.enable = true;
}
