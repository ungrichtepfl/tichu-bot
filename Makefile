package := tichu-bot
cli_exe := tichu-cli
gui_exe := tichu-gui

wasm_exe := tichu-wasm
wasm_dir := wasm
ghc_version_num := 9.12.2
ghc_version := ghc-$(ghc_version_num)
ghc_wasm_version_num := $(ghc_version_num).20250327
pkg_version := 0.1.1.0

ghc_wasm_env := "$$HOME/.ghc-wasm/env"

CFLAGS := -Wall -Werror -Wextra -Wpedantic -Wno-overlength-strings -ggdb -std=c23 -pedantic
CSRCDIR := gui
CMAIN := $(CSRCDIR)/main.c
CSRC := $(CSRCDIR)/gui.c
CINCLUDE := -I./$(CSRCDIR)/raylib-5.0/linux_amd64/include
CLFLAGS := -L./$(CSRCDIR)/raylib-5.0/linux_amd64/lib -l:libraylib.a -lm
COUT := cbuild
CPP := $(COUT)/gui_pp.c
CEXE := $(COUT)/main

.PHONY: all
all: cli-build gui-build

.PHONY: cli-build
cli-build: $(package).cabal
	cabal build -w$(ghc_version) $(package):exe:$(cli_exe)

.PHONY: cli-run
cli-run: $(package).cabal
	cabal run -w$(ghc_version) $(cli_exe)

.PHONY: gui-build
gui-build: c-pp $(package).cabal
	cabal build -w$(ghc_version) $(package):exe:$(gui_exe)

.PHONY: gui-run
gui-run: c-pp $(package).cabal
	cabal run -w$(ghc_version) $(gui_exe)

.PHONY: wasm-build
wasm-build: $(package).cabal wasm-c-gui-build
	. $(ghc_wasm_env) && wasm32-wasi-cabal build $(wasm_exe)
	cp "dist-newstyle/build/wasm32-wasi/ghc-$(ghc_wasm_version_num)/$(package)-$(pkg_version)/x/$(wasm_exe)/build/$(wasm_exe)/$(wasm_exe).wasm" $(wasm_dir)/
	. $(ghc_wasm_env) && "$$(wasm32-wasi-ghc --print-libdir)"/post-link.mjs -i $(wasm_dir)/$(wasm_exe).wasm -o $(wasm_dir)/ghc_wasm_jsffi.js

.PHONE: wasm-run
wasm-run: wasm-build
	python3 -m http.server --directory $(wasm_dir)

$(package).cabal: package.yaml
	hpack

$(COUT):
	mkdir -p $@

.PHONY: c-pp
c-pp: | $(COUT) # NOTE: This is needed as cabal does not recompile the source file if some header changed
	cc -P -E $(CFLAGS) $(CSRC) -o $(CPP) $(CINCLUDE)

.PHONY: c-build
c-build: | $(COUT)
	cc $(CFLAGS) $(CMAIN) -o $(CEXE) $(CINCLUDE) $(CLFLAGS)

.PHONY: c-run
c-run: c-build
	./$(CEXE)

.PHONY: compiledb
compiledb:
	compiledb make c-build

EMCC_FLAGS := -sUSE_GLFW=3 -sUSE_LIBPNG -sMODULARIZE=1 -sEXPORT_ES6=1 -sWASM=1 -sINITIAL_HEAP=256mb
EMCC_FLAGS := $(EMCC_FLAGS) --embed-file ./gui/images/
EMCC_FLAGS := $(EMCC_FLAGS) -sEXPORT_NAME=createTichuGui
EMCC_FLAGS := $(EMCC_FLAGS) -sEXPORTED_FUNCTIONS=_get_user_action,_update_c_state_and_render_game,_update_draw_config,_init,_deinit,_new_round,_game_should_stop,_should_game_restart,_malloc,_free,_send_mouse_button_pressed
EMCC_FLAGS := $(EMCC_FLAGS) -sEXPORTED_RUNTIME_METHODS=UTF8ToString,writeArrayToMemory
EXE_WEB := $(COUT)/tichu_gui.js
CMAIN_WEB := $(CSRCDIR)/gui.c

EMCC := emcc

CINCLUDE_WEB := -I./$(CSRCDIR)/raylib-5.0/wasm/include/
CLFLAGS_WEB := -L./$(CSRCDIR)/raylib-5.0/wasm/lib -l:libraylib.a
CFLAGS_WEB := -Oz -Wno-limited-postlink-optimizations

.PHONY: wasm-c-gui-build
wasm-c-gui-build: | $(COUT)
	$(EMCC) $(CFLAGS) $(CFLAGS_WEB) $(CMAIN_WEB) -o $(EXE_WEB) $(CINCLUDE_WEB) $(CLFLAGS_WEB) $(EMCC_FLAGS)
	cp $(EXE_WEB) $(EXE_WEB:$(COUT)/%.js=$(COUT)/%.wasm) $(wasm_dir)

.PHONY: clean
clean:
	rm -rf ./dist-newstyle
	cabal clean

.PHONY: update-index
update-index:
	cabal update
	. $(ghc_wasm_env) && wasm32-wasi-cabal update

.PHONY: test
test: $(package).cabal
	cabal test
