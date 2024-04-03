{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [ ansible ];

  shellHook = ''
    export PY_COLORS='1'
    export ANSIBLE_FORCE_COLOR='1'
  '';
}
