{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Control.Monad.Loops
import Foreign.C.String

foreign import ccall "update_draw" c_updateDraw :: IO ()
foreign import ccall "init" c_init :: IO ()
foreign import ccall "deinit" c_deinit :: IO ()
foreign import ccall "WindowShouldClose" c_windowShouldClose :: IO Bool

main :: IO ()
main = c_init >> whileM_ notStop c_updateDraw >> c_deinit
    where
        notStop = do
                shouldClose <- c_windowShouldClose
                return $ not shouldClose

-- () >>= (\b -> return $ not b))
