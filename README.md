# pluton

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
cabal run
```