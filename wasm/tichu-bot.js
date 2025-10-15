import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";
import createTichuGui from "./tichu_gui.js";

function getScaledCanvasCoordinates(clientX, clientY, canvas) {
  const rect = canvas.getBoundingClientRect(); // Get canvas position
  const scaleX = canvas.width / rect.width;
  const scaleY = canvas.height / rect.height;

  // Convert touch to canvas coordinates
  const canvasX = (clientX - rect.left) * scaleX;
  const canvasY = (clientY - rect.top) * scaleY;
  return [canvasX, canvasY];
}

let Module = {
  canvas: (function () {
    const canvas = document.getElementById("canvas");
    return canvas;
  })(),
};

let hs_tichu = undefined;
let gui = undefined;

document
  .getElementById("canvas")
  .addEventListener("touchstart", function (event) {
    if (event.touches.length > 0) {
      // Check if there is at least one touch point
      const touch = event.touches[0]; // Get first touch
      const [canvasX, canvasY] = getScaledCanvasCoordinates(
        touch.clientX,
        touch.clientY,
        this,
      );
      gui._send_mouse_button_pressed(canvasX, canvasY);
    }
    event.preventDefault(); // Prevent scrolling while touching
  });

document
  .getElementById("canvas")
  .addEventListener("mousedown", function (event) {
    if (event.button === 0) {
      // Left button pressed
      const [canvasX, canvasY] = getScaledCanvasCoordinates(
        event.clientX,
        event.clientY,
        this,
      );
      gui._send_mouse_button_pressed(canvasX, canvasY);
    }
  });

async function main() {
  gui = await createTichuGui(Module);
  function deinit() {
    gui._deinit();
  }

  function init(seed) {
    gui._init(seed);
  }

  function getUserAction() {
    const res = gui._get_user_action();
    const s = Module.UTF8ToString(res);
    const encoder = new TextEncoder();
    const bytes = encoder.encode(s + "\0"); // null-terminate
    const ptr = hs_tichu.malloc(bytes.length);
    new Uint8Array(hs_tichu.memory.buffer, ptr, bytes.length).set(bytes);
    return ptr;
  }

  function updateCStateAndRenderGame(toSend) {
    const haskellMem = new Uint8Array(hs_tichu.memory.buffer);
    let str_len = 0;
    while (haskellMem[toSend + str_len] !== 0) str_len++;
    const guiPtr = gui._malloc(str_len + 1);
    const haskell_str = haskellMem.subarray(toSend, toSend + str_len + 1);
    Module.writeArrayToMemory(haskell_str, guiPtr);
    gui._update_c_state_and_render_game(guiPtr);
    gui._free(guiPtr);
  }

  function updateDrawConfig() {
    const res = gui._update_draw_config();
    const s = Module.UTF8ToString(res);
    const encoder = new TextEncoder();
    const bytes = encoder.encode(s + "\0"); // null-terminate
    const ptr = hs_tichu.malloc(bytes.length);
    new Uint8Array(hs_tichu.memory.buffer, ptr, bytes.length).set(bytes);
    return ptr;
  }

  function newRound() {
    gui._new_round();
  }

  function gameShouldStop() {
    return gui._game_should_stop();
  }

  function shouldGameRestart() {
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
    let gameConfig = null;
    let gameConfigArgs = initialGameConfigArgs;
    while (true) {
      gameConfigArgs = await hs_tichu.configLoopBody(gameConfigArgs);
      if (await hs_tichu.configLoopStop(gameConfigArgs)) {
        gameConfig = JSON.stringify(JSON.parse(gameConfigArgs)[0]);
        break;
      }
    }
    let gameLoopArgs = await hs_tichu.initialGame(gameConfig, seed);
    while (true) {
      gameLoopArgs = await hs_tichu.gameLoopBody(gameLoopArgs);
      if (await hs_tichu.gameLoopStop(gameLoopArgs)) {
        return;
      }
    }
  }

  await tichuMain();
}

main();
