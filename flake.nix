{
  description = "Darwin's workstation setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      # url = "github:nix-community/home-manager";
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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
      };

      stateVersion = "24.11";
      username = "darwin";
    in {
      # Linux setup
      nixosConfigurations = {
        nixos-sophie = nixpkgs.lib.nixosSystem {
          inherit system;

          modules = [
            ./nixos/desktop/sophie/configuration.nix
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.darwin = import ./nixos/common/linux/home.nix {
                  inherit self pkgs stateVersion system;
                };
              };
            }
          ];
        };
      };

      # TODO: macOS setup
    };
}
