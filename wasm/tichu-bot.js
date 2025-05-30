import { WASI } from "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const consoleEl = document.getElementById("console");

const jsffiExports = {};

// Only 4 elements in the array
const out_queue = []
// Set up WASI
const wasi = new WASI({
  stdout: (out) => {
    out_queue.push(out);
    if (out_queue.length > 2) {
      out_queue.shift();
    }
    consoleEl.textContent += out;
    consoleEl.scrollTop = consoleEl.scrollHeight;
    // console.log(out);
  },
  stdin: () => {
    const out = out_queue[0];
    let p = window.prompt(out);
    if (p === null) {
      p = "0"; // Quit symbol
    }
    p += "\n";
    return p;
  }
});


// Run WASM
const wasm = await WebAssembly.instantiateStreaming(fetch("./tichu-wasm.wasm"),
  Object.assign(
    { ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports) },
    wasi.getImportObject()
  )
);
Object.assign(jsffiExports, wasm.instance.exports);
wasi.initialize(wasm, {
  ghc_wasm_jsffi: ghc_wasm_jsffi(jsffiExports)
});
// Wait 200 then run the function
setTimeout(() => {
  const updateGame = (game, playersAction) => {
    if (!game.shouldStop) {
      wasi.instance.exports.updateGame(JSON.stringify(game),
        JSON.stringify(playersAction)).then((jsonGameOutput) => {
          console.log(jsonGameOutput);
          const gameOutput = JSON.parse(jsonGameOutput);
          if (gameOutput[1] == null) {
            setTimeout(() => updateGame(gameOutput[0], gameOutput[1]), 1);
          }
        })
    }
  };
  const gameConfig = {
    sittingOrder: ["P1", "P2", "P3", "P4"],
    teamNames: ["Team 1", "Team 2"],
    scoreLimit: 1000,
  };
  wasi.instance.exports.newGame(JSON.stringify(gameConfig)).then((jsonGame) => {
    const [game, awailableActions] = JSON.parse(jsonGame);
    console.log(game);
    console.log(awailableActions);
    const action = ["P1", { tag: "Pass" }]
    updateGame(game, action);
  })
}, 200);
