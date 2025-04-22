# Compile to WASM

Install `wasm32-wasi-ghc` from [here](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta).
Use `export FLAVOUR=9.12`. Other versions are not tested.

Be aware that you need enough RAM to install it. If it does not work apply the
patch `setup.sh.patch.txt` to the setup.sh script in the repo above. Then use
`TEMP_DIR=$HOME ./setup.sh`.

Then run

```shell
./build.sh
```

and open `localhost:8000` in your browser.

Useful links:

- [A detailed guide to using GHC's WebAssembly backend](https://finley.dev/blog/2024-08-24-ghc-wasm.html)
