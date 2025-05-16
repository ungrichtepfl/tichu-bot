package:=tichu-bot
cli_exe:=tichu-cli

wasm_exe:=tichu-wasm
wasm_dir:=wasm
ghc_wasm_version:=9.12.2.20250327
pkg_version:=0.1.1.0


.PHONY: cli-build
cli-build:
	stack build $(package):exe:$(cli_exe)

.PHONY: cli-run
cli-run:
	stack run $(package):exe:$(cli_exe)

.PHONY: wasm-build
wasm-build:
	stack build --only-configure # HACK: generates the cabal file
	python3 ./patch_cabal.py $(wasm_exe) ./$(package).cabal # HACK: patches cabal file to work with wasm32-wasi-cabal
	wasm32-wasi-cabal build $(wasm_exe)

.PHONE: wasm-run
wasm-run: wasm-build
	cp "dist-newstyle/build/wasm32-wasi/ghc-$(ghc_wasm_version)/$(package)-$(pkg_version)/x/$(wasm_exe)/build/$(wasm_exe)/$(wasm_exe).wasm" $(wasm_dir)/
	"$$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i $(wasm_dir)/$(wasm_exe).wasm -o $(wasm_dir)/ghc_wasm_jsffi.js
	python3 -m http.server --directory $(wasm_dir)
