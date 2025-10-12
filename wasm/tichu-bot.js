import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";
import createTichuGui from "./tichu_gui.js";

var Module = {
  canvas: (function () {
    var canvas = document.getElementById("canvas");
    return canvas;
  })(),
};

var hs_tichu = undefined;

async function main() {
  const gui = await createTichuGui(Module);
  function deinit() {
    console.log("deinit");
    gui._deinit();
  }

  function init(seed) {
    console.log("init: ", seed);
    gui._init(seed);
  }

  function getUserAction() {
    console.log("getUserAction");
    return gui._get_user_action();
  }

  function updateCStateAndRenderGame(toSend) {
    console.log(typeof toSend);
    console.log("updateCStateAndRenderGame");
    gui._update_c_state_and_render_game(toSend);
  }

  function updateDrawConfig() {
    const res = gui._update_draw_config();
    var s = Module.UTF8ToString(res);
    const encoder = new TextEncoder();
    const bytes = encoder.encode(s + "\0"); // null-terminate
    const ptr = hs_tichu.malloc(bytes.length);
    new Uint8Array(hs_tichu.memory.buffer, ptr, bytes.length).set(bytes);
    return ptr;
  }

  function newRound() {
    console.log("newRound");
    gui._new_round();
  }

  function gameShouldStop() {
    return gui._game_should_stop();
  }

  function shouldGameRestart() {
    console.log("shouldGameRestart");
    return gui._should_game_restart();
  }

  const jsffiExports = {};
  const customExports = {
    env: {
      init,
      deinit,
      getUserAction,
      updateCStateAndRenderGame,
      updateDrawConfig,
      newRound,
      gameShouldStop,
      shouldGameRestart,
    },
  };

  // Set up WASI
  const wasi = new WASI({
    stdout: (out) => {
      console.log(out);
    },
    stdin: () => {
      const out = out_queue[0];
      let p = window.prompt(out);
      if (p === null) {
        p = "";
      }
      p += "\n";
      return p;
    },
  });

  // Run WASM
  const wasm = await WebAssembly.instantiateStreaming(
    fetch("./tichu-wasm.wasm"),
    {
      ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports),
      ...wasi.getImportObject(),
      ...customExports,
    },
  );
  Object.assign(jsffiExports, wasm.instance.exports);
  wasi.initialize(wasm, {
    ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports),
  });

  hs_tichu = wasi.instance.exports;
  const initialGameConfigArgs = JSON.stringify([null, false]);
  const seed = 0;
  const userPlayerIndex = 2;

  async function tichuMain() {
    await hs_tichu.gameInit(userPlayerIndex);
    var gameConfig = null;
    var gameConfigArgs = initialGameConfigArgs;
    while (true) {
      gameConfigArgs = await hs_tichu.configLoopBody(gameConfigArgs);
      if (await hs_tichu.configLoopStop(gameConfigArgs)) {
        gameConfig = JSON.stringify(JSON.parse(gameConfigArgs)[0]);
        break;
      }
    }
    console.log("End Config Loop");
    var gameLoopArgs = await hs_tichu.initialGame(gameConfig, seed);
    while (true) {
      gameLoopArgs = await hs_tichu.gameLoopBody(gameLoopArgs);
      if (await hs_tichu.gameLoopStop(gameLoopArgs)) {
        return;
      }
      // setTimeout(() => {}, 10);
    }
  }

  await tichuMain();
}

main();
