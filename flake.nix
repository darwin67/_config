{
  description = "Darwin's workstation setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Timed wallpaper
    timewall.url = "github:bcyran/timewall";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        config = {
          allowUnfree = true;
          allowBroken = false;
        };

        inherit system;

        overlays = [ inputs.timewall.overlays.default ];
      };

      stateVersion = "24.11";
      username = "darwin";
      wallpaperTheme = "macMonterey";
    in {
      # Linux setup
      nixosConfigurations = {
        nixos-sophie = let
          additionalFiles = {
            ".config/sway/config.d/screen.conf" = {
              source = "${self}/nixos/desktop/sophie/sway/screen.conf";
            };
          };
        in nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit pkgs; };

          modules = [
            ./nixos/desktop/sophie/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.darwin = import ./nixos/common/linux/home.nix {
                  inherit self inputs pkgs username wallpaperTheme stateVersion
                    system additionalFiles;
                };
              };
            }
          ];
        };

        framework = let additionalFiles = { };
        in nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit pkgs; };

          modules = [
            ./nixos/laptop/framework13/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                extraSpecialArgs = { inherit pkgs; };
                users.darwin = import ./nixos/common/linux/home.nix {
                  inherit self inputs pkgs username wallpaperTheme stateVersion
                    system additionalFiles;
                };
              };
            }
          ];
        };
      };

      # TODO: macOS setup
    };
}
