# pluton

[![Hercules-ci][Herc badge]][Herc link]
[![Cachix Cache][Cachix badge]][Cachix link]

[Herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[Herc link]: https://hercules-ci.com/github/Plutonomicon/pluton
[Cachix badge]: https://img.shields.io/badge/cachix-public--plutonomicon-blue.svg
[Cachix link]: https://public-plutonomicon.cachix.org 

**Work in Progress**

Pluton is intended to:

- Write example smart contracts using [Plutarch](https://github.com/Plutonomicon/plutarch) to assess what's needed to be done at the eDSL layer.
- Benchmark (script size, cpu/mem cost) functions and smart contracts written in Haskell, Plutarch and [Pluto](https://github.com/Plutonomicon/pluto)

See [Project board](https://github.com/orgs/Plutonomicon/projects/2/views/1) for planned tasks.

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

If you use ghcid, you may find `bin/{ghcid,test}` handy.

## Running the tests

In a nix-shell, run:

```
cabal run pluton
```

## Benchmarks

Note: Benchmarks are work in progress; we intend to benchmark all examples in CI.

```
cabal bench
```

This will write the benchmark report to `bench.*`.

### Benchmarking a commit

To run benchmarks on a particular commit,

```
nix run github:Plutonomicon/pluton/<COMMIT-GOES-HERE>#benchmark
```
