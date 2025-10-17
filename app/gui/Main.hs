{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Foreign.C.String (CString)
import System.Random (randomRIO)

import qualified Data.Aeson as AS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Foreign.C.String as CS

import Game.Interface
import Game.Structures
import Game.Tichu

foreign import ccall "get_user_action" c_getUserAction :: IO CString
foreign import ccall "update_c_state_and_render_game" c_updateCStateAndRenderGame :: CString -> IO ()
foreign import ccall "update_draw_config" c_updateDrawConfig :: IO CString
foreign import ccall "init" c_init :: Int -> IO ()
foreign import ccall "deinit" c_deinit :: IO ()
foreign import ccall "new_round" c_newRound :: IO ()
foreign import ccall "game_should_stop" c_gameShouldStop :: IO Bool
foreign import ccall "should_game_restart" c_shouldGameRestart :: IO Bool

data CInterface = CInterface

instance Interface CInterface where
    gameInit _ = c_init
    gameDeinit _ = c_deinit
    updateDrawConfig _ = c_updateDrawConfig >>= getGameConfig
    gameShouldStop _ = c_gameShouldStop
    shouldGameRestart _ = c_shouldGameRestart
    updateStateAndRenderGame _ toSend = withCAString toSend c_updateCStateAndRenderGame
    getUserAction _ = c_getUserAction >>= getCurrentUserAction
    newRound _ = c_newRound

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

getCurrentUserAction :: CString -> IO (Maybe PlayerAction)
getCurrentUserAction cAction = do
    decoded <- makeHsType cAction
    case decoded of
        Just actions -> return actions
        Nothing ->
            do
                cActionString <- CS.peekCAString cAction
                c_deinit
                error $ "ERROR: Wrong players action.\nPlayer Action: " ++ cActionString

randomStarts :: Int
randomStarts = 100000

main :: IO ()
main =
    do
        let interface = CInterface
        gameInit interface userPlayerIndex
        mConfig <- configLoop interface
        seed <- randomRIO (0, randomStarts)
        case mConfig of
            Just config ->
                gameLoop interface seed config >> gameDeinit interface
            Nothing -> gameDeinit interface
