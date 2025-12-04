{ config, lib, pkgs, ... }:

{
  imports = [ ../../common/apple/conf.nix ../../common/apple/pkg.nix ];

  homebrew = {
    casks = [ "slack" "zoom" "notion" "linear-linear" "tailscale-app" ];
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

    awscli2
  ];

  environment.shellAliases = { kubectl = "kubecolor"; };
}
