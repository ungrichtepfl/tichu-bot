{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Control.Monad.Loops
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as AS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import Data.Maybe (isJust)
import Foreign.C.String (CString)
import qualified Foreign.C.String as CS
import Game.Structures
import Game.Tichu

foreign import ccall "update_draw" c_updateDraw :: CString -> IO CString
foreign import ccall "update_draw_config" c_updateDrawConfig :: IO CString
foreign import ccall "init" c_init :: IO ()
foreign import ccall "deinit" c_deinit :: IO ()
foreign import ccall "window_should_close" c_windowShouldClose :: IO Bool

withCAString :: (ToJSON a) => a -> (CString -> IO b) -> IO b
withCAString t =
    let s = BL.unpack $ AS.encode t
     in CS.withCAString s

makeHsType :: (FromJSON a) => CString -> IO (Maybe a)
makeHsType cs = do
    s <- CS.peekCAString cs
    return $ AS.decode $ BL.pack s

getGameConfig :: CString -> IO (Maybe GameConfig)
getGameConfig cGameConfig = do
    decoded <- makeHsType cGameConfig
    case decoded of
        Just gameConf -> return gameConf
        Nothing ->
            do
                cGameConfigString <- CS.peekCAString cGameConfig
                c_deinit
                error $ "ERROR: Wrong game config.\nGame Config: " ++ cGameConfigString

getCurrentAction :: CString -> IO (Maybe (PlayerName, PlayerAction))
getCurrentAction cAction = do
    decoded <- makeHsType cAction
    case decoded of
        Just actions -> return actions
        Nothing ->
            do
                cActionString <- CS.peekCAString cAction
                c_deinit
                error $ "ERROR: Wrong players action.\nPlayer Action: " ++ cActionString

main :: IO ()
main =
    let
        shouldStopConfigLoop configOutput = isJust (fst configOutput) || snd configOutput
        configLoop :: (Maybe GameConfig, Bool) -> IO (Maybe GameConfig, Bool)
        configLoop _ = do
            cConf <- c_updateDrawConfig
            mConf <- getGameConfig cConf
            end <- c_windowShouldClose
            return (mConf, end)
        shouldStopGameLoop gameOutput = shouldGameStop (fst gameOutput)
        gameLoop :: (Game, Maybe (Map PlayerName [PlayerAction])) -> IO (Game, Maybe (Map PlayerName [PlayerAction]))
        gameLoop gameOutput =
            let game = fst gameOutput
             in do
                    cAction <- withCAString gameOutput c_updateDraw
                    action <- getCurrentAction cAction
                    let gameOutput' = updateGame game action
                    let game' = fst gameOutput'
                    end <- c_windowShouldClose
                    let game'' = game'{shouldGameStop = end || shouldGameStop game}
                    return (game'', snd gameOutput')
     in
        do
            c_init
            (mConfig, _) <- iterateUntilM shouldStopConfigLoop configLoop (Nothing, False)
            case mConfig of
                Just config ->
                    iterateUntilM shouldStopGameLoop gameLoop (newGame config 0) >> c_deinit
                Nothing -> c_deinit
