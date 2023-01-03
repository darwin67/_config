# Config files and dotfiles

Settings that are shared across computers.

## Post language installation

Run the following script to install the common libraries used.

```bash
setup
```

### Elixir

``` bash
export MIX_ENV=prod
git clone git@github.com:elixir-lsp/elixir-ls.git
mix deps.get
mix compile
mix elixir_ls.release -o ~/.lsp/elixir
```

Add `~/.lsp/elixir` to `$PATH` and run `doom env` to make sure Doom know about that
path.
