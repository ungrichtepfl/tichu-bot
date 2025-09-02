{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import GHC.Wasm.Prim (JSString)

import qualified Data.Aeson as AS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified GHC.Wasm.Prim as WP

import Game.Interface
import Game.Structures
import Game.Tichu

foreign export javascript "gameLoopBody" gameLoopBodyJS :: JSString -> IO JSString

gameLoopBodyJS :: JSString -> IO JSString
gameLoopBodyJS jsString =
    let interface = JInterface
     in do
            args <- getGameLoopBodyArg jsString
            res <- gameLoopBody interface args
            return $ makeJsString res

foreign import javascript unsafe "getUserAction()" j_getUserAction :: IO JSString
foreign import javascript unsafe "updateCStateAndRenderGame($1)" j_updateCStateAndRenderGame :: JSString -> IO ()
foreign import javascript unsafe "updateDrawConfig($1)" j_updateDrawConfig :: IO JSString
foreign import javascript unsafe "init($1)" j_init :: Int -> IO ()
foreign import javascript unsafe "deinit($1)" j_deinit :: IO ()
foreign import javascript unsafe "newRound()" j_newRound :: IO ()
foreign import javascript unsafe "gameShouldStop()" j_gameShouldStop :: IO Bool
foreign import javascript unsafe "shouldGameRestart()" j_shouldGameRestart :: IO Bool

makeJsString :: (ToJSON a) => a -> JSString
makeJsString = WP.toJSString . BL.unpack . AS.encode

makeHsType :: (FromJSON a) => JSString -> Maybe a
makeHsType = AS.decode . BL.pack . WP.fromJSString

getGameLoopBodyArg :: JSString -> IO (Game, Maybe (Map PlayerName [PlayerAction]))
getGameLoopBodyArg jArgs =
    let decoded = makeHsType jArgs
     in case decoded of
            Just args -> return args
            Nothing ->
                do
                    let jArgsString = WP.fromJSString jArgs
                    j_deinit
                    error $ "ERROR: Wrong game config.\nGame Config: " ++ jArgsString

getGameConfig :: JSString -> IO (Maybe GameConfig)
getGameConfig jGameConfig =
    let decoded = makeHsType jGameConfig
     in case decoded of
            Just gameConf -> return gameConf
            Nothing ->
                do
                    let jGameConfigString = WP.fromJSString jGameConfig
                    j_deinit
                    error $ "ERROR: Wrong game config.\nGame Config: " ++ jGameConfigString

getCurrentUserAction :: JSString -> IO (Maybe PlayerAction)
getCurrentUserAction jAction =
    let decoded = makeHsType jAction
     in case decoded of
            Just actions -> return actions
            Nothing ->
                do
                    let jActionString = WP.fromJSString jAction
                    j_deinit
                    error $ "ERROR: Wrong players action.\nPlayer Action: " ++ jActionString

data JInterface = JInterface

instance Interface JInterface where
    gameInit _ = j_init
    gameDeinit _ = j_deinit
    updateDrawConfig _ = j_updateDrawConfig >>= getGameConfig
    gameShouldStop _ = j_gameShouldStop
    shouldGameRestart _ = j_shouldGameRestart
    updateStateAndRenderGame _ toSend = j_updateCStateAndRenderGame (makeJsString toSend)
    getUserAction _ = j_getUserAction >>= getCurrentUserAction
    newRound _ = j_newRound

-- NOTE: Needed such that it compiles
main :: IO ()
main = return ()
