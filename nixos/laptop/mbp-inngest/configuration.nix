{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../../common/apple/conf.nix
    ../../common/apple/pkg.nix
  ];

  homebrew = {
    taps = [ "depot/tap" ];

    brews = [
      "depot/tap/depot"
    ];

    casks = [
      "slack"
      "zoom"
      "notion"
      "linear"
      "tailscale-app"
    ];
  };

  environment.systemPackages = with pkgs; [
    kubectl
    kubectl-rook-ceph
    kubectl-tree
    kubectl-ktop
    kubectl-graph
    kubectl-explore
    kubectl-images
    kubectl-node-shell
    kubecolor
    krew
    cilium-cli

    awscli2
  ];

  environment.shellAliases = {
    kubectl = "kubecolor";
  };
}
