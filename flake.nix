{
  description = "Pruned version of the Racket package catalog";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";

    write-catalog.url = "path:write-catalog.rkt";
    write-catalog.flake = false;

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, write-catalog, ...}:
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        l = pkgs.lib // builtins;

        pruner = pkgs.writeScriptBin "prune-catalog" ''
          #!/usr/bin/env bash
          set -eou pipefail
          ${pkgs.racket}/bin/racket -e '(require (file "${write-catalog}")) (write-catalog "$1")'
        '';
      in
        {
          packages.default = pruner;
          defaultApp = pruner;
        }
    );
}
