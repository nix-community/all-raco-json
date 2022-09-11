{
  description = "JSON representation of the Racket package catalog";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        l = pkgs.lib // builtins;

        converter = pkgs.writeScriptBin "convert-catalog-to-json" ''
          #!/usr/bin/env bash
          set -eou pipefail
          ${pkgs.racket}/bin/racket -e '(require (file "convert.rkt"))'
        '';
      in
        {
          packages.default = converter;
          defaultApp = converter;
        }
    );
}
