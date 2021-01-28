let
  # this is probably a hackish way of getting a suitable development environment 
  pkgs = import <nixpkgs> { };
  ocamlDevel =
    with pkgs.ocamlPackages; [ merlin pkgs.ocamlformat ];
  base = (import ./default.nix { });
in
base.overrideAttrs (attrs: {
  buildInputs =
    attrs.buildInputs ++ base.devInputs ++ ocamlDevel;
})
