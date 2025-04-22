module Main where

import Cli (playTichu)

foreign export javascript "playTichu" playTichu :: IO ()

main :: IO ()
main = return ()
