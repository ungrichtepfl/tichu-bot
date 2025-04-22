#!/bin/sh

set -ex

out_dir="dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/tichu-bot-0.1.1.0/x/tichu-bot-wasm/build/tichu-bot-wasm/tichu-bot-wasm.wasm" 

wasm32-wasi-cabal build tichu-bot-wasm
cp "$out_dir" .
"$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i tichu-bot-wasm.wasm -o ghc_wasm_jsffi.js 
python -m http.server 
