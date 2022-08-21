{
  description = "JSON representation of the Racket package catalog";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    racket-catalog.url = "path:./pkgs-all";
    racket-catalog.flake = false;

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        l = pkgs.lib // builtins;

        # TODO: should we worry about acquiring sha256?

        converter = pkgs.writeScriptBin "convert-catalog-to-json" ''
          #!/usr/bin/env bash
          set -eou pipefail
          ${pkgs.racket}/bin/racket -e '(require (file "convert.rkt")) (to-json "pkgs-all" "pkgs-all.json")'
        '';
      in
        {
          packages.default = converter;
          defaultApp = converter;
        }
    );
}
