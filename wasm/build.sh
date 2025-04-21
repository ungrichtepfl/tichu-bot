#!/bin/sh

set -ex

out_dir="dist-newstyle/build/wasm32-wasi/ghc-9.8.4.20250206/tichu-bot-0.1.1.0/x/tichu-bot-wasm/build/tichu-bot-wasm/tichu-bot-wasm.wasm" 

wasm32-wasi-cabal build tichu-bot-wasm
cp "$out_dir" .
python -m http.server 
