{
  description = "pluton";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    plutus.url = "github:input-output-hk/plutus"; # used for libsodium-vrf (TODO: Is this really needed?)
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, plutus, ... }:
    let
      supportedSystems = with nixpkgs.lib.systems.supported; tier1 ++ tier2 ++ tier3;
      perSystem = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };

      projectFor = system:
        let pkgs = nixpkgsFor system; in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          cabalProjectLocal = builtins.readFile ./cabal-haskell.nix.project;
          modules = [{
            packages = {
              marlowe.flags.defer-plugin-errors = true;
              plutus-use-cases.flags.defer-plugin-errors = true;
              plutus-ledger.flags.defer-plugin-errors = true;
              plutus-contract.flags.defer-plugin-errors = true;
              cardano-crypto-praos.components.library.pkgconfig =
                nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig =
                nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
            };
          }];
          shell = {
            withHoogle = true;
            exactDeps = true;

            nativeBuildInputs = [
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.haskellPackages.cabal-fmt
              pkgs.hlint
              pkgs.ghcid
            ];

            tools = {
              cabal = { };
              fourmolu = { };
              haskell-language-server = { }; # Must use haskell.nix, because the compiler version should match
            };

            additional = ps: [
              ps.base-deriving-via
              ps.cardano-addresses
              ps.cardano-addresses-cli
              ps.cardano-binary
              ps.cardano-crypto
              ps.cardano-crypto-class
              ps.cardano-crypto-praos
              ps.cardano-crypto-wrapper
              ps.cardano-ledger-alonzo
              ps.cardano-ledger-byron
              ps.cardano-ledger-core
              ps.cardano-ledger-pretty
              ps.cardano-ledger-shelley
              ps.cardano-ledger-shelley-ma
              ps.cardano-prelude
              ps.cardano-slotting
              ps.flat
              ps.freer-extras
              ps.goblins
              ps.measures
              ps.orphans-deriving-via
              ps.playground-common
              ps.plutus-contract
              ps.plutus-core
              ps.plutus-ledger
              ps.plutus-ledger-api
              ps.plutus-pab
              ps.plutus-playground-server
              ps.plutus-tx
              ps.plutus-tx-plugin
              ps.plutus-use-cases
              ps.prettyprinter-configurable
              ps.quickcheck-dynamic
              ps.Win32-network
              ps.word-array
              ps.pluto
              ps.plutarch
            ];
          };

          # sha256 hashes for repos pinned in cabal-haskell.nix.project
          sha256map = {
            "https://github.com/input-output-hk/plutus.git"."ae5f8b3ff64766717694bdca3d0385e717a963b8" = "cUStRRQRMa55/auopJHsphAcNNO5GgLHjyAI8HitE/I=";
            "https://github.com/input-output-hk/plutus-apps.git"."ff17e42be4ede4ff9711634f1b924ca1de90851a" = "u8gU/uQYNDA0KM6+hNI3Oo0qOfjBx04b30I/Zsbv2Pc=";
            "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
            "https://github.com/input-output-hk/purescript-bridge.git"."366fc70b341e2633f3ad0158a577d52e1cd2b138" = "paaId4GJ9/Z5LstYfakiCJZ2p9Q5NMHXdXUx5rTPQKI=";
            "https://github.com/input-output-hk/servant-purescript.git"."ebea59c7bdfc0338d83fca772b9a57e28560bcde" = "VkM9Q2XkDEnQh6khptoIjQ9xW7Fc2wsOJ4vPYDzBTD4=";
            "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" = "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
            "https://github.com/input-output-hk/cardano-base"."4ea7e2d927c9a7f78ddc69738409a5827ab66b98" = "zbjq43Bnhv1/LhJCFlI8gdd61dGvVlkEa6wkCvLqEFg=";
            "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
            "https://github.com/input-output-hk/cardano-addresses"."d2f86caa085402a953920c6714a0de6a50b655ec" = "XgXQKJHRKAFwIjONh19D/gKE0ARlhMXXcV74eZpd0lw=";
            "https://github.com/j-mueller/cardano-wallet"."6be73ab852c0592713dfe78218856d4a8a0ee69e" = "5IZuqlE/4aGH3TEuGYQsZwOpI/Q7DYzJ4q3stuqGpWc=";
            "https://github.com/input-output-hk/ouroboros-network"."1f4973f36f689d6da75b5d351fb124d66ef1057d" = "lwTgyoZBQAaU6Sh7BouGJGUvK1tSVrWhJP63v7MpwKA=";
            "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" = "QE3QRpIHIABm+qCP/wP4epbUx0JmSJ9BMePqWEd3iMY=";
            "https://github.com/input-output-hk/cardano-ledger-specs"."bf008ce028751cae9fb0b53c3bef20f07c06e333" = "HTPOmVOXgBD/3uAxZip/HSttaKcJ+uImYDbuwANAw1c=";
            "https://github.com/input-output-hk/cardano-node.git"."b6ca519f97a0e795611a63174687e6bb70c9f752" = "tuEtSCJOk1MA9sguxL13XLa+qHaz//v7eNyhxHC9tHw=";
            "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" = "uQx+SEYsCH7JcG3xAT0eJck9yq3y0cvx49bvItLLer8=";
            "https://github.com/input-output-hk/Win32-network"."2d1a01c7cbb9f68a1aefe2934aad6c70644ebfea" = "sha256-uvYEWalN62ETpH45/O7lNHo4rAIaJtYpLWdIcAkq3dA=";
            "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "z9ut0y6umDIjJIRjz9KSvKgotuw06/S8QDwOtVdGiJ0=";
            "https://github.com/Plutonomicon/pluto"."6546ff776ba811966af3a975938ada69c61f01ec" = "sha256-J1AHljKWjlmNMz/VfPxZ13e/f5S5fmlUW0b9jTTugAY";
            "https://github.com/Plutonomicon/plutarch"."9c3c4d9af4ac312cd916353c1c31692bac3f7a85" = "sha256-milRRiKO0JBZNnnLyEs0sFkWUDvT78reAYMLJtuKxrM=";
          };
        };
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system};
          } "touch $out"
      );
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);

      # For Hercules CI
      nixCi = inputs.flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        # Add darwin here once Plutus supports it.
        systems = [ "x86_64-linux" ];
      };
    };
}
