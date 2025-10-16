import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghcWasmJsffi from "./ghc_wasm_jsffi.js";
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

const CModule = {
  canvas: (function () {
    return document.getElementById("canvas");
  })(),
};

let hsExports = undefined;
let cExports = undefined;

async function tichuMain() {
  cExports = await createTichuGui(CModule);

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
        cExports._send_mouse_button_pressed(canvasX, canvasY);
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
        cExports._send_mouse_button_pressed(canvasX, canvasY);
      }
    });

  function deinit() {
    cExports._deinit();
  }

  function init(seed) {
    cExports._init(seed);
  }

  function getUserAction() {
    const cPtr = cExports._get_user_action();
    const jsStr = CModule.UTF8ToString(cPtr);
    const encoder = new TextEncoder();
    const strBytes = encoder.encode(jsStr + "\0"); // null-terminate
    const hsPtr = hsExports.malloc(strBytes.length);
    new Uint8Array(hsExports.memory.buffer, hsPtr, strBytes.length).set(
      strBytes,
    );
    return hsPtr;
  }

  function updateCStateAndRenderGame(toSend) {
    const hsMemory = new Uint8Array(hsExports.memory.buffer);
    let strLen = 0;
    while (hsMemory[toSend + strLen] !== 0) strLen++;
    const cPtr = cExports._malloc(strLen + 1);
    const hsBytes = hsMemory.subarray(toSend, toSend + strLen + 1);
    CModule.writeArrayToMemory(hsBytes, cPtr);
    cExports._update_c_state_and_render_game(cPtr);
    cExports._free(cPtr);
  }

  function updateDrawConfig() {
    const cPtr = cExports._update_draw_config();
    const jsStr = CModule.UTF8ToString(cPtr);
    const encoder = new TextEncoder();
    const strBytes = encoder.encode(jsStr + "\0"); // null-terminate
    const hsPtr = hsExports.malloc(strBytes.length);
    new Uint8Array(hsExports.memory.buffer, hsPtr, strBytes.length).set(
      strBytes,
    );
    return hsPtr;
  }

  function newRound() {
    cExports._new_round();
  }

  function gameShouldStop() {
    return cExports._game_should_stop();
  }

  function shouldGameRestart() {
    return cExports._should_game_restart();
  }

  const hsJsffiExports = {};
  const hsCustomExports = {
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
  const hsWasi = new WASI({
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
  const hsWasm = await WebAssembly.instantiateStreaming(
    fetch("./tichu-wasm.wasm"),
    {
      ghc_wasm_jsffi: ghcWasmJsffi(hsJsffiExports),
      ...hsWasi.getImportObject(),
      ...hsCustomExports,
    },
  );
  Object.assign(hsJsffiExports, hsWasm.instance.exports);
  hsWasi.initialize(hsWasm, {
    ghc_wasm_jsffi: ghcWasmJsffi(hsJsffiExports),
  });

  hsExports = hsWasi.instance.exports;

  const initialGameConfigArgs = JSON.stringify([null, false]);
  const randomStarts = 100_000;
  const seed = Math.floor(Math.random() * randomStarts);
  const userPlayerIndex = 2;

  async function tichuGameLoop() {
    await hsExports.gameInit(userPlayerIndex);
    let gameConfig = null;
    let gameConfigArgs = initialGameConfigArgs;
    while (true) {
      gameConfigArgs = await hsExports.configLoopBody(gameConfigArgs);
      if (await hsExports.configLoopStop(gameConfigArgs)) {
        gameConfig = JSON.stringify(JSON.parse(gameConfigArgs)[0]);
        break;
      }
    }
    let gameLoopArgs = await hsExports.initialGame(gameConfig, seed);
    while (true) {
      gameLoopArgs = await hsExports.gameLoopBody(gameLoopArgs);
      if (await hsExports.gameLoopStop(gameLoopArgs)) {
        return;
      }
    }
  }

  await tichuGameLoop();
}

tichuMain();
