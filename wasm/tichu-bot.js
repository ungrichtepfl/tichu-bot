import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

function deinit() {
  console.log("deinit");
}

function init(seed) {
  console.log("init: ", seed);
}

function getUserAction() {
  console.log("getUserAction");
  return "nu";
}

function updateCStateAndRenderGame(toSend) {
  console.log(type(toSend));
  console.log("updateCStateAndRenderGame");
}

function updateDrawConfig() {
  console.log("updateDrawConfig");
  return "nu";
}

function newRound() {
  console.log("newRound");
}

function gameShouldStop() {
  console.log("gameShouldStop");
  return false;
}

function shouldGameRestart() {
  console.log("shouldGameRestart");
  return false;
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

const hs_tichu = wasi.instance.exports;
const initialGameConfigArgs = JSON.stringify([null, false]);
const seed = 0;
const userPlayerIndex = 2;

function tichuMain() {
  hs_tichu.gameInit(userPlayerIndex);
  var gameConfig = "null";
  var gameConfigArgs = initialGameConfigArgs;
  while (gameConfig === "null") {
    gameConfigArgs = hs_tichu.configLoopBody(gameConfigArgs);
    if (hs_tichu.configLoopStop(gameConfigArgs)) {
      return;
    }
    gameConfig = JSON.stringify(JSON.parse(gameConfigArgs)[0]);
    setTimeOut(() => {}, 10);
  }
  var gameLoopArgs = hs_tichu.initialGame(gameConfig, seed);
  while (true) {
    gameLoopArgs = hs_tichu.gameLoopBody(gameLoopArgs);
    if (hs_tichu.gameLoopStop(gameLoopArgs)) {
      return;
    }
    setTimeOut(() => {}, 10);
  }
}

tichuMain();
