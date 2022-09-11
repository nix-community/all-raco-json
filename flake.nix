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

        pruner = pkgs.writeScriptBin "prune catalog" ''
          #!/usr/bin/env bash
          set -eou pipefail
          ${pkgs.racket}/bin/racket -e '(require (file "convert.rkt")) (write-catalog "pkgs-all")'
        '';
      in
        {
          packages.default = pruner;
          defaultApp = pruner;
        }
    );
}
