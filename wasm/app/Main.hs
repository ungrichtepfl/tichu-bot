module Main where

import Jsi (setupTichu, updateTichu)
import GHC.Wasm.Prim

foreign export javascript "updateTichu" updateTichu :: JSString -> IO JSString
foreign export javascript "setupTichu" setupTichu :: IO JSString

main :: IO ()
main = return ()
