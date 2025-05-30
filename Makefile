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
	cp "dist-newstyle/build/wasm32-wasi/ghc-$(ghc_wasm_version)/$(package)-$(pkg_version)/x/$(wasm_exe)/build/$(wasm_exe)/$(wasm_exe).wasm" $(wasm_dir)/
	"$$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i $(wasm_dir)/$(wasm_exe).wasm -o $(wasm_dir)/ghc_wasm_jsffi.js

.PHONE: wasm-run
wasm-run: wasm-build
	python3 -m http.server --directory $(wasm_dir)

CFLAGS:= -Wall -Werror -Wextra -Wpedantic -Wno-overlength-strings -ggdb -std=c23 -pedantic
CSRC:=gui/gui.c
COUT_OBJ:=cout/gui.o
COUT_EXE:=cout/gui
COUT:=cout

.PHONY: c-gui-build-obj
c-gui-build-obj:
	cc $(CFLAGS) $(CSRC) -c -o $(COUT_OBJ) -I./gui/raylib-5.0/linux_amd64/include -I/usr/include

.PHONY: c-gui-build
c-gui-build: c-gui-build-obj
	cc $(CFLAGS) $(COUT_OBJ) -o $(COUT_EXE) -L./gui/raylib-5.0/linux_amd64/lib -l:libraylib.a -lm


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
