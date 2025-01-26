{
  description = "Darwin's workstation setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Timed wallpaper
    timewall.url = "github:bcyran/timewall";
    zen-browser.url = "github:youwen5/zen-browser-flake";
    ghostty.url = "github:ghostty-org/ghostty";
    sops-nix.url = "github:Mic92/sops-nix";
    # MacOS
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, home-manager, flake-utils, sops-nix
    , nix-darwin, ... }:
    let
      username = "darwin";
      stateVersion = "24.11";
      wallpaperTheme = "macMonterey";

      # Function for helping configuration linux systems
      mkLinuxSystem = { system ? "x86_64-linux", modules ? [ ]
        , additionalFiles ? { }, ... }: {
          specialArgs = { inherit system; };

          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config = {
              allowUnfree = true;
              allowBroken = false;
            };

            overlays = [
              inputs.timewall.overlays.default
              inputs.ghostty.overlays.default

              (final: prev: {
                inherit (inputs.zen-browser.packages."${system}") zen-browser;
              })
            ];
          };

          modules = modules ++ [
            sops-nix.nixosModules.sops
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.darwin = import ./nixos/common/linux/home.nix {
                  inherit self inputs username wallpaperTheme stateVersion
                    additionalFiles;
                };
              };
            }
          ];
        };

      ## list of hosts
      hosts = {
        sophie = mkLinuxSystem {
          modules = [ ./nixos/desktop/sophie/configuration.nix ];
          additionalFiles = {
            ".config/sway/config.d/screen.conf" = {
              source = "${self}/nixos/desktop/sophie/sway/screen.conf";
            };
          };
        };

        xps15-7590 = mkLinuxSystem {
          modules = [ ./nixos/laptop/xps15-7590/configuration.nix ];
          additionalFiles = {
            ".config/sway/config.d/screen.conf" = {
              source = "${self}/nixos/laptop/xps15-7590/sway/screen.conf";
            };
          };
        };

        framework13 = mkLinuxSystem {
          modules = [ ./nixos/laptop/framework13/configuration.nix ];
        };

        thinkpadz16 = mkLinuxSystem {
          modules = [ ./nixos/laptop/thinkpadz16/configuration.nix ];
          additionalFiles = {
            ".config/sway/config.d/screen.conf" = {
              source = "${self}/nixos/laptop/thinkpadz16/sway/screen.conf";
            };
          };
        };
      };

      # Linux setup
      nixosConfigurations = if nixpkgs.stdenv.isLinux then {
        sophie = nixpkgs.lib.nixosSystem (hosts.sophie);
        framework = nixpkgs.lib.nixosSystem (hosts.framework13);
        xps15-7590 = nixpkgs.lib.nixosSystem (hosts.xps15-7590);
        ThinkpadZ16-NixOS = nixpkgs.lib.nixosSystem (hosts.thinkpadz16);
      } else
        { };

      # TODO: macOS setup
      darwinConfigurations = {
        "Darwins-Mac-mini" = nix-darwin.lib.darwinSystem {
          specialArgs = { inherit self inputs; };

          pkgs = import nixpkgs {
            system = "aarch64-darwin";
            config.allowUnfree = true;
          };
          modules = [ ./nixos/desktop/m4mini/configuration.nix ];
        };
      };

    in {
      inherit nixosConfigurations darwinConfigurations;

      # devShells."${system}".default = pkgs.mkShell {
      #   buildInputs = with pkgs; [
      #     sops
      #     age
      #     yamllint
      #     nodePackages.yaml-language-server
      #   ];
      # };
    };
}
