# Compile to WASM

Install `wasm32-wasi-ghc` from [here](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta).
Use `export FLAVOUR=9.8`. Somehow I could not download the others.

Then run

```shell
./build.sh
```

and open `localhost:8000` in your browser.

Useful links:

- [A detailed guide to using GHC's WebAssembly backend](https://finley.dev/blog/2024-08-24-ghc-wasm.html)
