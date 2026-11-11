{
  description = "Darwin's workstation setup";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote/v1.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llm-agents.url = "github:numtide/llm-agents.nix";
    genkan = {
      url = "github:darwin67/genkan";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Strata needs a newer Rust than our release nixpkgs provides.
    strata = {
      url = "github:lgse/strata/v1.2.1";
    };
    # Desktop shell for Niri; tracks upstream main for features
    # newer than nixpkgs' release.
    noctalia = {
      url = "github:noctalia-dev/noctalia";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Noctalia plugins, pinned here instead of auto-updated by the shell.
    noctalia-official-plugins = {
      url = "github:noctalia-dev/official-plugins";
      flake = false;
    };
    noctalia-plugins = {
      url = "github:noctalia-dev/community-plugins";
      flake = false;
    };
    # AirPods daemon (librepods fork) for the AirPods Noctalia plugin.
    omarchy-pods = {
      url = "github:thisisgm/omarchy-pods";
      flake = false;
    };
    # CLI behind the AI Usage Noctalia plugin; not in nixpkgs.
    ai-usagebar = {
      url = "github:akitaonrails/ai-usagebar";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # MacOS
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-26.05-darwin";
    nix-darwin = {
      url = "github:LnL7/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs-darwin";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-darwin,
      home-manager,
      sops-nix,
      nix-darwin,
      ...
    }:
    let
      username = "darwin";
      stateVersion = "26.05";

      # Function for helping configuration linux systems
      mkLinuxSystem =
        {
          system ? "x86_64-linux",
          modules ? [ ],
          additionalFiles ? { },
          homeModule ? ./nixos/common/desktop/home.nix,
          ...
        }:
        let
          pkgs = import nixpkgs {
            inherit system;

            config = {
              allowUnfree = true;
              allowBroken = false;
            };
          };
        in
        {
          specialArgs = { inherit system inputs; };

          modules = modules ++ [
            sops-nix.nixosModules.sops
            home-manager.nixosModules.home-manager
            # Base settings shared by all Linux hosts (desktop and server).
            ./nixos/common/base/conf.nix
            ./nixos/common/base/pkg.nix
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
                    stateVersion
                    home-manager
                    ;
                  inherit additionalFiles;
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
                  inherit additionalFiles;
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
          additionalFiles = {
            ".config/niri/host.kdl" = {
              source = "${self}/nixos/laptop/framework16/niri/host.kdl";
            };
          };
        };

        m4mini = mkMacOSSystem {
          modules = [ ./nixos/desktop/m4mini/configuration.nix ];
        };

        mbp-inngest = mkMacOSSystem {
          modules = [ ./nixos/laptop/mbp-inngest/configuration.nix ];
          additionalFiles = {
            ".config/amp/settings.json" = {
              force = true;
              text = builtins.toJSON {
                "amp.git.commit.ampThread.enabled" = false;
                "amp.git.commit.coauthor.enabled" = false;
                "amp.mcpServers" = {
                  linear.url = "https://mcp.linear.app/mcp";
                  notion.url = "https://mcp.notion.com/mcp";
                };
              };
            };
          };
        };
      };
    in
    {
      # Linux setup
      nixosConfigurations = {
        sophie = nixpkgs.lib.nixosSystem (hosts.sophie);
        framework16 = nixpkgs.lib.nixosSystem (hosts.framework16);
      };

      # macOS setup
      darwinConfigurations = {
        "Darwins-Mac-mini" = nix-darwin.lib.darwinSystem (hosts.m4mini);
        "Darwin-MBP-Inngest" = nix-darwin.lib.darwinSystem (hosts.mbp-inngest);
      };
    }
    // {
      devShells =
        nixpkgs.lib.genAttrs
          [
            "x86_64-linux"
            "aarch64-linux"
            "aarch64-darwin"
            "x86_64-darwin"
          ]
          (
            system:
            let
              pkgs = import nixpkgs {
                inherit system;

                config = {
                  allowUnfree = true;
                };
              };

            in
            {
              default = pkgs.mkShell {
                buildInputs =
                  (with pkgs; [
                    sops
                    age
                    yamllint
                    yaml-language-server
                  ])
                  ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
                    inputs.llm-agents.packages.${system}.claude-code
                    inputs.llm-agents.packages.${system}.opencode
                  ];
              };
            }
          );
    };
}
