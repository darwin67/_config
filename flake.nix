{
  description = "Darwin's workstation setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    nixos-hardware.url = "github:NixOS/nixos-hardware";

    lanzaboote = {
      url = "github:nix-community/lanzaboote/v1.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Timed wallpaper
    timewall = {
      url = "github:bcyran/timewall";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    tuigreet = {
      url = "github:NotAShelf/tuigreet";
      flake = false;
    };
    crane.url = "github:ipetkov/crane";

    codex-cli-nix.url = "github:sadjow/codex-cli-nix";

    # MacOS
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-26.05-darwin";
    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };

    # dev
    # use the latest available to get packages
    # NOTE: this might not be needed later, more so that claude is locked to an
    # old version and cannot be started
    nixpkgs-dev.url = "github:nixos/nixpkgs?ref=master";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-darwin,
      home-manager,
      flake-utils,
      sops-nix,
      nix-darwin,
      nixpkgs-dev,
      nixos-hardware,
      lanzaboote,
      ...
    }:
    let
      username = "darwin";
      stateVersion = "26.05";
      wallpaperTheme = "macMonterey";

      # Function for helping configuration linux systems
      mkLinuxSystem =
        {
          system ? "x86_64-linux",
          modules ? [ ],
          additionalFiles ? { },
          includeKinesis ? true,
          homeModule ? ./nixos/common/linux/home.nix,
          ...
        }:
        let
          pkgs = import nixpkgs {
            inherit system;

            config = {
              allowUnfree = true;
              allowBroken = false;
            };

            overlays = [ inputs.timewall.overlays.default ];
          };
          baseAdditionalFiles = nixpkgs.lib.optionalAttrs includeKinesis {
            ".config/sway/config.d/kinesis-freestyle-keyboard.conf" = {
              source = "${self}/sway/config.d/kinesis-freestyle-keyboard.conf";
            };
          };
        in
        {
          specialArgs = { inherit system inputs; };

          modules = modules ++ [
            sops-nix.nixosModules.sops
            home-manager.nixosModules.home-manager
            {
              nixpkgs.pkgs = pkgs;
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.darwin = import homeModule {
                  inherit
                    self
                    inputs
                    pkgs
                    username
                    wallpaperTheme
                    stateVersion
                    home-manager
                    ;
                  additionalFiles = baseAdditionalFiles // additionalFiles;
                };
              };
            }
          ];
        };

      mkLinuxServerSystem =
        {
          system ? "x86_64-linux",
          modules ? [ ],
          additionalFiles ? { },
          ...
        }:
        mkLinuxSystem {
          inherit system additionalFiles;
          includeKinesis = false;
          homeModule = ./nixos/common/server/home.nix;
          modules = [
            ./nixos/common/server/conf.nix
            ./nixos/common/server/pkg.nix
          ]
          ++ modules;
        };

      mkMacOSSystem =
        {
          system ? "aarch64-darwin",
          modules ? [ ],
          additionalFiles ? { },
          includeKinesis ? true,
          ...
        }:
        let
          pkgs = import nixpkgs-darwin {
            inherit system;

            config = {
              allowUnfree = true;
              allowBroken = false;
            };
          };
          baseAdditionalFiles = nixpkgs.lib.optionalAttrs includeKinesis {
            ".config/sway/config.d/kinesis-freestyle-keyboard.conf" = {
              source = "${self}/sway/config.d/kinesis-freestyle-keyboard.conf";
            };
          };
        in
        {
          specialArgs = {
            inherit
              self
              system
              inputs
              username
              ;
          };

          modules = modules ++ [
            home-manager.darwinModules.home-manager
            {
              nixpkgs.pkgs = pkgs;
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.darwin = import ./nixos/common/apple/home.nix {
                  inherit
                    self
                    pkgs
                    inputs
                    username
                    stateVersion
                    home-manager
                    ;
                  additionalFiles = baseAdditionalFiles // additionalFiles;
                  # inherit (nixpkgs) lib;
                };
              };
            }
          ];
        };

      ## list of hosts
      hosts = {
        sophie = mkLinuxServerSystem {
          modules = [ ./nixos/desktop/sophie/configuration.nix ];
        };

        framework16 = mkLinuxSystem {
          modules = [ ./nixos/laptop/framework16/configuration.nix ];
          includeKinesis = false;
          additionalFiles = {
            ".config/sway/config.d/override.conf" = {
              source = "${self}/nixos/laptop/framework16/sway/override.conf";
            };
          };
        };

        m4mini = mkMacOSSystem {
          modules = [ ./nixos/desktop/m4mini/configuration.nix ];
        };

        mbp-inngest = mkMacOSSystem {
          modules = [ ./nixos/laptop/mbp-inngest/configuration.nix ];
        };
      };
    in
    {
      # Linux setup
      nixosConfigurations = {
        sophie = nixpkgs.lib.nixosSystem (hosts.sophie);
        framework16 = nixpkgs.lib.nixosSystem (hosts.framework16);
        thinkpad-z16 = nixpkgs.lib.nixosSystem (hosts.thinkpadz16);
      };

      # macOS setup
      darwinConfigurations = {
        "Darwins-Mac-mini" = nix-darwin.lib.darwinSystem (hosts.m4mini);
        "Darwin-MBP-Inngest" = nix-darwin.lib.darwinSystem (hosts.mbp-inngest);
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs-dev {
          inherit system;

          config = {
            allowUnfree = true;
          };
        };

      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            sops
            age
            yamllint
            yaml-language-server

            claude-code
            opencode
          ];
        };
      }
    );
}
