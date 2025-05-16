
.PHONY: build-cli
build-cli:
	stack build tichu-bot:exe:tichu-cli

.PHONY: run-cli
run-cli:
	stack run tichu-bot:exe:tichu-cli

.PHONY: build-wasm
build-wasm:
	stack build --only-configure # HACK: generates the cabal file
	python3 ./patch_cabal.py tichu-wasm ./tichu-bot.cabal # HACK: patches cabal file to work with wasm32-wasi-cabal
	wasm32-wasi-cabal build tichu-wasm

.PHONE: run-wasm
run-wasm: build-wasm
	cp "dist-newstyle/build/wasm32-wasi/ghc-9.12.2.20250327/tichu-bot-0.1.1.0/x/tichu-wasm/build/tichu-wasm/tichu-wasm.wasm" wasm/
	"$$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i wasm/tichu-wasm.wasm -o wasm/ghc_wasm_jsffi.js
	python3 -m http.server --directory wasm
