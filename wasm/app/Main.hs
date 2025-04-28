module Main where

import GHC.Wasm.Prim
import Jsi (newGame, updateGame)

foreign export javascript "updateGame" updateGame :: JSString -> JSString -> JSString
foreign export javascript "newGame" newGame :: JSString -> JSString

main :: IO ()
main = return ()
