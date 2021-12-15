# plut

Compare Pluto and Plutarch, via small examples and a sample contract

## Developing

If you are developing with VSCode, [`nix-direnv`](https://github.com/nix-community/nix-direnv) is recommended. Using home-manager, you can install it as follows:

```nix
programs.direnv = {
  enable = true;
  enableBashIntegration = true;
  nix-direnv.enable = true;
};
```