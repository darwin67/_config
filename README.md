# Config files and dotfiles

Settings that are shared across computers.

## NixOS setup

```sh
# back it up
sudo mv /etc/nixos /etc/nixos.old

sudo ln -s ~/_config /etc/nixos
```

## macOS setup

```sh
sudo ln -s ~/_config /etc/nix-darwin

darwin-rebuild switch
```

## Daily LLM package updates

The **Update LLM packages** GitHub Actions workflow checks daily at 14:23 UTC
(06:23 PST / 07:23 PDT), or manually through **Run workflow**. It updates the
`llm-agents` flake input (Claude Code, OpenCode, Amp CLI, and Codex) and the
Linux Amp Desktop pin from Amp's official Debian repository.

Changes go to one PR on `automation/update-llm-packages`; subsequent runs refresh
that PR instead of creating duplicates. No changes means no new PR. Updates
must pass the updater tests, `nix flake check`, and builds of the four agents
and Amp Desktop on x86_64 Linux before publication. Review and merge manually;
the job never pushes to the default branch, merges PRs, or applies a system
configuration. macOS Homebrew packages are outside this job's scope.

The schedule starts after this workflow is merged into the default branch.
The workflow requests its own write permissions; enable **Allow GitHub Actions
to create and approve pull requests** in the repository's Actions settings.
It uses `GITHUB_TOKEN`, so no extra secret is needed; its bot-created PRs do not
trigger other workflows. Validation runs in the update job before the PR is
created or refreshed.
