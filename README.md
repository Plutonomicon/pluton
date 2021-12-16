# plut

**Work in Progress**

Compare [Pluto](https://github.com/Plutonomicon/pluto) and [Plutarch](https://github.com/Plutonomicon/plutarch), via small examples and sample contracts, along the axis of developer ergonomics as well as generated script size.

## Developing

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