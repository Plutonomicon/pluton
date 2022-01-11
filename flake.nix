{
  description = "pluton";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    # haskell.nix
    haskell-nix.url = "github:input-output-hk/haskell.nix?rev=4aeeba8d713d0b98c92c8c717df24da17d463c1d";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";

    # Flakes & CI infra
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-utils.url = "github:numtide/flake-utils";

    # Plutus inputs 
    plutus.url = "github:input-output-hk/plutus?rev=ae5f8b3ff64766717694bdca3d0385e717a963b8";
    plutus-apps.url = "github:input-output-hk/plutus-apps?rev=ff17e42be4ede4ff9711634f1b924ca1de90851a";
    cardano-prelude.url = "github:input-output-hk/cardano-prelude?rev=fd773f7a58412131512b9f694ab95653ac430852";
    cardano-prelude.flake = false;
    cardano-base.url = "github:input-output-hk/cardano-base?rev=4ea7e2d927c9a7f78ddc69738409a5827ab66b98";
    cardano-base.flake = false;
    cardano-crypto.url = "github:input-output-hk/cardano-crypto?rev=07397f0e50da97eaa0575d93bee7ac4b2b2576ec";
    cardano-crypto.flake = false;
    cardano-node.url = "github:input-output-hk/cardano-node?rev=b6ca519f97a0e795611a63174687e6bb70c9f752";
    cardano-node.flake = false;
    cardano-ledger-specs.url = "github:input-output-hk/cardano-ledger-specs?rev=bf008ce028751cae9fb0b53c3bef20f07c06e333";
    cardano-ledger-specs.flake = false;
    cardano-wallet.url = "github:j-mueller/cardano-wallet?rev=6be73ab852c0592713dfe78218856d4a8a0ee69e";
    cardano-wallet.flake = false;
    cardano-addresses.url = "github:input-output-hk/cardano-addresses?rev=d2f86caa085402a953920c6714a0de6a50b655ec";
    cardano-addresses.flake = false;
    ouroboros-network.url = "github:input-output-hk/ouroboros-network?rev=1f4973f36f689d6da75b5d351fb124d66ef1057d";
    ouroboros-network.flake = false;
    # ee59880f47ab835dbd73bea0847dab7869fc20d8
    flat.url = "github:Quid2/flat?rev=d32c2c0c0c3c38c41177684ade9febe92d279b06";
    flat.flake = false;
    # 2d1a01c7cbb9f68a1aefe2934aad6c70644ebfea
    Win32-network.url = "github:input-output-hk/Win32-network?rev=3825d3abf75f83f406c1f7161883c438dac7277d";
    Win32-network.flake = false;
    iohk-monitoring-framework.url = "github:input-output-hk/iohk-monitoring-framework?rev=46f994e216a1f8b36fe4669b47b2a7011b0e153c";
    iohk-monitoring-framework.flake = false;
    goblins.url = "github:input-output-hk/goblins?rev=cde90a2b27f79187ca8310b6549331e59595e7ba";
    goblins.flake = false;
    optparse-applicative.url = "github:input-output-hk/optparse-applicative?rev=7497a29cb998721a9068d5725d49461f2bba0e7a";
    optparse-applicative.flake = false;

    # Our project dependenties
    pluto.url = "github:Plutonomicon/pluto";
    pluto.flake = false;
    plutarch.url = "github:Plutonomicon/plutarch/5b2688f20d41ca042083c7830086ea79bd63f4af";
    plutarch.flake = false;
    Shrinker.url = "github:Plutonomicon/Shrinker";
    Shrinker.flake = false;
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, flake-utils, plutus, flake-compat, flake-compat-ci, ... }:
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
          extraSources = [
            {
              src = inputs.cardano-prelude;
              subdirs = [
                "cardano-prelude"
                "cardano-prelude-test"
              ];
            }
            {
              src = inputs.cardano-base;
              subdirs = [
                "base-deriving-via"
                "binary"
                "binary/test"
                "cardano-crypto-class"
                "cardano-crypto-praos"
                "cardano-crypto-tests"
                "measures"
                "orphans-deriving-via"
                "slotting"
                "strict-containers"
              ];
            }
            {
              src = inputs.cardano-crypto;
              subdirs = [ "." ];
            }
            {
              src = inputs.Win32-network;
              subdirs = [ "." ];
            }
            {
              src = inputs.flat;
              subdirs = [ "." ];
            }
            {
              src = inputs.iohk-monitoring-framework;
              subdirs = [
                "iohk-monitoring"
                "tracer-transformers"
                "contra-tracer"
                "plugins/backend-aggregation"
                "plugins/backend-ekg"
                "plugins/backend-monitoring"
                "plugins/backend-trace-forwarder"
                "plugins/scribe-systemd"
              ];
            }
            {
              src = inputs.plutus;
              subdirs = [
                "plutus-benchmark"
                "plutus-core"
                "plutus-errors"
                "plutus-ledger-api"
                "plutus-metatheory"
                "plutus-tx"
                "plutus-tx-plugin"
                "prettyprinter-configurable"
                "word-array"
                "stubs/plutus-ghc-stub"
              ];
            }
            {
              src = inputs.plutus-apps;
              subdirs = [
                "freer-extras"
                "playground-common"
                "plutus-chain-index"
                "plutus-chain-index-core"
                "plutus-contract"
                "plutus-ledger"
                "plutus-pab"
                "plutus-playground-server"
                "plutus-use-cases"
                "quickcheck-dynamic"
                "web-ghc"
              ];
            }
            {
              src = inputs.cardano-ledger-specs;
              subdirs = [
                "byron/ledger/impl"
                "cardano-ledger-core"
                "cardano-protocol-tpraos"
                "eras/alonzo/impl"
                "eras/byron/chain/executable-spec"
                "eras/byron/crypto"
                "eras/byron/crypto/test"
                "eras/byron/ledger/executable-spec"
                "eras/byron/ledger/impl/test"
                "eras/shelley/impl"
                "eras/shelley-ma/impl"
                "eras/shelley/chain-and-ledger/executable-spec"
                "eras/shelley/test-suite"
                "shelley/chain-and-ledger/shelley-spec-ledger-test"
                "libs/non-integral"
                "libs/small-steps"
                "libs/cardano-ledger-pretty"
                "semantics/small-steps-test"
              ];
            }
            {
              src = inputs.cardano-wallet;
              subdirs = [
                "lib/text-class"
                "lib/strict-non-empty-containers"
                "lib/core"
                "lib/test-utils"
                "lib/numeric"
                "lib/launcher"
                "lib/core-integration"
                "lib/cli"
                "lib/shelley"
              ];
            }
            {
              src = inputs.cardano-addresses;
              subdirs = [
                "core"
                "command-line"
              ];
            }
            {
              src = inputs.cardano-node;
              subdirs = [
                "cardano-api"
                "cardano-node"
                "cardano-cli"
                "cardano-config"
              ];
            }
            {
              src = inputs.optparse-applicative;
              subdirs = [ "." ];
            }
            {
              src = inputs.ouroboros-network;
              subdirs = [
                "monoidal-synchronisation"
                "typed-protocols"
                "typed-protocols-cborg"
                "typed-protocols-examples"
                "ouroboros-network"
                "ouroboros-network-testing"
                "ouroboros-network-framework"
                "ouroboros-consensus"
                "ouroboros-consensus-byron"
                "ouroboros-consensus-cardano"
                "ouroboros-consensus-shelley"
                "io-sim"
                "io-classes"
                "network-mux"
                "ntp-client"
              ];
            }
            {
              src = inputs.goblins;
              subdirs = [ "." ];
            }
            {
              src = inputs.pluto;
              subdirs = [ "." ];
            }
            {
              src = inputs.plutarch;
              subdirs = [ "." ];
            }
            {
              src = inputs.Shrinker;
              subdirs = [ "." "testing" ];
            }
          ];
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
              pkgs.cabal-install
              pkgs.hlint
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.ghcid
              # pkgs.cabal-fmt
              pkgs.haskellPackages.cabal-fmt
            ];

            tools = {
              haskell-language-server = { }; # Must use haskell.nix, because the compiler version should match
              # cabal-fmt = { };
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
        };
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);

      # TODO: add check
      # https://github.com/Plutonomicon/plutarch/blob/master/flake.nix

      apps = perSystem (system: self.flake.${system}.apps // {
        benchmark = {
          type = "app";
          program = "${self.flake.packages."pluton:bench:perf"}/bin/perf";
        };
        # Add more apps here ...
      });
      devShell = perSystem (system: self.flake.${system}.devShell);

      nixCi = flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };
      # We need to append the checks that come from haskell.nix to our own,
      # hence the need for flake.checks // {}.
      checks = perSystem (system: self.flake.${system}.checks // {
        benchmark = (nixpkgsFor system).runCommand "benchmark" { } "${self.apps.${system}.benchmark.program} | tee $out";
      });
      ciNix = inputs.flake-compat-ci.lib.recurseIntoFlakeWith {
        flake = self;
        systems = [ "x86_64-linux" ];
      };
      # defaultPackage = flake.packages."pluton:exe:pluton";
    };
}
