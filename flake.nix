{
  description = "quaalude";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let packageName = "quaalude";
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        project = pkgs.haskellPackages.developPackage {
          root = ./quaalude;
          name = packageName;
        };

        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in
      {
        defaultPackage = self.packages.${system}.${packageName};

        packages = {
          "${packageName}" = project;

          testConfigurations =
            let

              inherit (pkgs.haskell.lib) dontCheck;
              makeTestConfiguration =
                let defaultPkgs = pkgs;
                in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
                  let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
                  in (pkgs.haskell.packages.${ghcVersion}.override (old: {
                    overrides = combineOverrides old [
                      (packageSourceOverrides { quaalude = ./quaalude; })
                      (new: old: { })
                      overrides
                    ];
                  })).quaalude;
            in
            rec {
              ghc-9-2 = makeTestConfiguration { ghcVersion = "ghc92"; };
              ghc-9-4 = makeTestConfiguration { ghcVersion = "ghc94"; };
              ghc-9-6 = makeTestConfiguration { ghcVersion = "ghc96"; };
              all = pkgs.symlinkJoin {
                name = "quaalude";
                paths = [ ghc-9-2 ghc-9-4 ghc-9-6 ];
              };
            };
        };
      });
}
