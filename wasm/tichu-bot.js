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
    console.log(out);
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
const wasm = await WebAssembly.instantiateStreaming(fetch("./tichu-bot-wasm.wasm"),
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
  wasi.instance.exports.playTichu();
}, 200);
