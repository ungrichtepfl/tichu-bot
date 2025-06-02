{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Control.Monad.Loops
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as AS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Foreign.C.String (CString)
import qualified Foreign.C.String as CS
import Game.Structures
import Game.Tichu

foreign import ccall "update_draw" c_updateDraw :: CString -> IO ()
foreign import ccall "init" c_init :: IO ()
foreign import ccall "deinit" c_deinit :: IO ()
foreign import ccall "WindowShouldClose" c_windowShouldClose :: IO Bool
foreign import ccall "get_current_action" c_getCurrentAction :: IO CString

withCAString :: (ToJSON a) => a -> (CString -> IO b) -> IO b
withCAString t =
    let s = BL.unpack $ AS.encode t
     in CS.withCAString s

makeHsType :: (FromJSON a) => CString -> IO (Maybe a)
makeHsType cs = do
    s <- CS.peekCAString cs
    return $ AS.decode $ BL.pack s

getCurrentAction :: IO (Maybe (PlayerName, PlayerAction))
getCurrentAction = do
    cAction <- c_getCurrentAction
    decoded <- makeHsType cAction
    case decoded of
        Just actions -> return actions
        Nothing ->
            do
                cActionString <- CS.peekCAString cAction
                error $ "ERROR: Wrong players action.\nPlayer Action: " ++ cActionString

config :: GameConfig
config =
    GameConfig
        { sittingOrder = ["P1", "P2", "P3", "P4"]
        , teamNames = ["Team 1", "Team 2"]
        , scoreLimit = 1000
        }

main :: IO ()
main =
    let initialGame = newGame config 0
        shouldStop gameOutput = shouldGameStop (fst gameOutput)
        gameLoop :: (Game, Maybe (Map PlayerName [PlayerAction])) -> IO (Game, Maybe (Map PlayerName [PlayerAction]))
        gameLoop gameOutput =
            let game = fst gameOutput
             in do
                    withCAString gameOutput c_updateDraw
                    action <- getCurrentAction
                    let gameOutput' = updateGame game action
                    let game' = fst gameOutput'
                    end <- c_windowShouldClose
                    let game'' = game'{shouldGameStop = end || shouldGameStop game}
                    return (game'', snd gameOutput')
     in c_init >> iterateUntilM shouldStop gameLoop initialGame >> c_deinit

-- () >>= (\b -> return $ not b))
