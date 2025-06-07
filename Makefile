package:=tichu-bot
cli_exe:=tichu-cli
gui_exe:=tichu-gui

wasm_exe:=tichu-wasm
wasm_dir:=wasm
ghc_version_num:=9.12.2
ghc_version:=ghc-$(ghc_version_num)
ghc_wasm_version_num:=$(ghc_version_num).20250327
pkg_version:=0.1.1.0

ghc_wasm_env:="$$HOME/.ghc-wasm/env"

.PHONY: cli-build
cli-build: cabal-config
	cabal build -w$(ghc_version) $(package):exe:$(cli_exe)

.PHONY: cli-run
cli-run: cabal-config
	cabal run -w$(ghc_version) $(cli_exe)

.PHONY: gui-build
gui-build: cabal-config
	cabal build -w$(ghc_version) $(package):exe:$(gui_exe)

.PHONY: gui-run
gui-run: cabal-config
	cabal run -w$(ghc_version) $(gui_exe)

.PHONY: wasm-build
wasm-build: cabal-config
	. $(ghc_wasm_env) && wasm32-wasi-cabal build $(wasm_exe) 
	cp "dist-newstyle/build/wasm32-wasi/ghc-$(ghc_wasm_version_num)/$(package)-$(pkg_version)/x/$(wasm_exe)/build/$(wasm_exe)/$(wasm_exe).wasm" $(wasm_dir)/
	. $(ghc_wasm_env) && "$$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i $(wasm_dir)/$(wasm_exe).wasm -o $(wasm_dir)/ghc_wasm_jsffi.js

.PHONE: wasm-run
wasm-run: wasm-build
	python3 -m http.server --directory $(wasm_dir)

.PHONY: cabal-config
cabal-config:
	hpack

CFLAGS:= -Wall -Werror -Wextra -Wpedantic -Wno-overlength-strings -ggdb -std=c23 -pedantic
CSRC:=gui/gui.c
COUT_EXE:=cout/gui
COUT:=cout

.PHONY: c-gui-build
c-gui-build:
	cc $(CFLAGS) $(CSRC) -o $(COUT_EXE) -I./gui/raylib-5.0/linux_amd64/include -L./gui/raylib-5.0/linux_amd64/lib -l:libraylib.a -lm

.PHONY: c-gui-run
c-gui-run: c-gui-build
	./cout/gui

.PHONY: compiledb
compiledb:
	compiledb make c-gui-build

EMCC_FLAGS := -sUSE_GLFW=3 -sUSE_LIBPNG -sASYNCIFY -sMODULARIZE=1 -sEXPORT_ES6=1 -sWASM=1 -sINITIAL_HEAP=256mb
EMCC_FLAGS := $(EMCC_FLAGS) --embed-file ./Lato-Regular.ttf --embed-file ./trained_network.txt
EMCC_FLAGS := $(EMCC_FLAGS) -sEXPORT_NAME=createDitect
EMCC_FLAGS := $(EMCC_FLAGS) -sEXPORTED_FUNCTIONS=_run_gui,_send_mouse_button_down,_send_mouse_button_released,_send_space_pressed,_send_rkey_pressed

.PHONY: wasm-gui-build
wasm-c-gui-build:
	exit 1 # TODO:

.PHONY: clean
clean:
	rm -rf ./dist-newstyle
	cabal clean

.PHONY: freeze
freeze:
	cabal freeze

