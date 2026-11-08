{
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    tpm2-tools
  ];
}
