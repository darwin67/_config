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
      stateVersion = "24.11";
      username = "darwin";
      wallpaperTheme = "macMonterey";

      # Function for helping configuration linux systems
      mkLinuxSystem = { system, modules ? [ ], additionalFiles ? { } }:
        let
          pkgs = import nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              allowBroken = false;
            };

            overlays = [ inputs.timewall.overlays.default ];
          };
        in {
          specialArgs = { inherit system pkgs; };

          modules = modules ++ [
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.darwin = import ./nixos/common/linux/home.nix {
                  inherit self inputs pkgs username wallpaperTheme stateVersion
                    additionalFiles;
                };
              };
            }
          ];
        };

      ## list of hosts
      hosts = {
        nixos-sophie = mkLinuxSystem {
          system = "x86_64-linux";
          modules = [ ./nixos/desktop/sophie/configuration.nix ];
          additionalFiles = {
            ".config/sway/config.d/screen.conf" = {
              source = "${self}/nixos/desktop/sophie/sway/screen.conf";
            };
          };
        };

        framework13 = mkLinuxSystem {
          system = "x86_64-linux";
          modules = [ ./nixos/laptop/framework13/configuration.nix ];
        };
      };
    in {
      # Linux setup
      nixosConfigurations = {
        nixos-sophie = nixpkgs.lib.nixosSystem (hosts.nixos-sophie);
        framework = nixpkgs.lib.nixosSystem (hosts.framework13);
      };

      # TODO: macOS setup
    };
}
