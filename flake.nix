{
  description = "Darwin's workstation setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Timed wallpaper
    timewall.url = "github:bcyran/timewall";
    zen-browser.url = "github:youwen5/zen-browser-flake";
    sops-nix.url = "github:Mic92/sops-nix";

    # MacOS
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.11-darwin";
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-darwin, home-manager, flake-utils
    , sops-nix, nix-darwin, ... }:
    let
      username = "darwin";
      stateVersion = "24.11";
      wallpaperTheme = "macMonterey";

      # Function for helping configuration linux systems
      mkLinuxSystem =
        { system ? "x86_64-linux", modules ? [ ], additionalFiles ? { }, ... }:
        let
          pkgs = import nixpkgs {
            inherit system;

            config = {
              allowUnfree = true;
              allowBroken = false;
            };

            overlays = [
              inputs.timewall.overlays.default

              (final: prev: {
                inherit (inputs.zen-browser.packages."${system}") zen-browser;
              })
            ];
          };
        in {
          specialArgs = { inherit system pkgs; };

          modules = modules ++ [
            sops-nix.nixosModules.sops
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

      mkMacOSSystem = { system ? "aarch64-darwin", modules ? [ ], ... }:
        let
          pkgs = import nixpkgs-darwin {
            inherit system;

            config = {
              allowUnfree = true;
              allowBroken = false;
            };
          };
        in {
          specialArgs = { inherit self system inputs username pkgs; };

          modules = modules ++ [
            home-manager.darwinModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.darwin = import ./nixos/common/apple/home.nix {
                  inherit self pkgs inputs username stateVersion;
                  # inherit (nixpkgs) lib;
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

        m4mini = mkMacOSSystem {
          modules = [ ./nixos/desktop/m4mini/configuration.nix ];
        };
      };
    in {
      # Linux setup
      nixosConfigurations = {
        sophie = nixpkgs.lib.nixosSystem (hosts.sophie);
        framework = nixpkgs.lib.nixosSystem (hosts.framework13);
        xps15-7590 = nixpkgs.lib.nixosSystem (hosts.xps15-7590);
        ThinkpadZ16-NixOS = nixpkgs.lib.nixosSystem (hosts.thinkpadz16);
      };

      # macOS setup
      darwinConfigurations = {
        "Darwins-Mac-mini" = nix-darwin.lib.darwinSystem (hosts.m4mini);
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };

      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sops
            age
            yamllint
            nodePackages.yaml-language-server
          ];
        };
      });
}
