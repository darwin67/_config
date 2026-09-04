{ inputs, pkgs }:

{
  home.packages = [ inputs.flox.packages.${pkgs.stdenv.hostPlatform.system}.default ];
}
