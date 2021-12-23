# pluton

[![Hercules-ci][Herc badge]][Herc link]
[![Cachix Cache][Cachix badge]][Cachix link]
[![Built with Nix][Built with Nix badge]][Built with Nix link]

[Herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[Herc link]: https://hercules-ci.com/github/Plutonomicon/pluton
[Cachix badge]: https://img.shields.io/badge/cachix-public--plutonomicon-blue.svg
[Cachix link]: https://public-plutonomicon.cachix.org 

**Work in Progress**

Pluton is intended to:

- enrich [Plutarch](https://github.com/Plutonomicon/plutarch) to enable a more ergonomic DSL for writing smart contracts. It is a staging ground for these features to eventually be upstreamed to Plutarch. 
- Benchmark (script size, cpu/mem cost) functions and smart contracts written in Haskell, Plutarch and [Pluto](https://github.com/Plutonomicon/pluto), and use that as a guide for enrichment.

## Developing

`nix develop` should get a dev environment with Haskell Language Server support and ghcid.

If you are developing with VSCode, [`nix-direnv`](https://github.com/nix-community/nix-direnv) is recommended. Using home-manager, you can install it as follows:

```nix
programs.direnv = {
  enable = true;
  enableBashIntegration = true;
  nix-direnv.enable = true;
};
```

## Running the tests

In a nix-shell, run:

```
cabal run pluton
```

## Benchmarks

Note: Benchmarks are not implemented yet. This is only a placeholder.

```
cabal bench
```

This will write the benchmark report to `report.html`.

### Benchmarking a commit

To run benchmarks on a particular commit,

```
nix run github:Plutonomicon/pluton/<COMMIT-GOES-HERE>#benchmark
```
