{ inputs, pkgs, ... }:

{
  # llm-agents.nix builds its packages from source, but publishes them to the
  # Numtide cache. The flake only auto-configures that cache when run directly,
  # so add it here to download prebuilt binaries instead of compiling them.
  nix.settings = {
    extra-substituters = [ "https://cache.numtide.com" ];
    extra-trusted-public-keys = [
      "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    ];
  };

  environment.systemPackages = with inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
    claude-code
    opencode
    amp
    codex
  ];
}
