{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Foreign (free)
import Foreign.C.String (CString)
import GHC.Wasm.Prim (JSString)

import qualified Data.Aeson as AS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Foreign.C.String as CS
import qualified GHC.Wasm.Prim as WP

import Game.Interface
import Game.Structures
import Game.Tichu

foreign export javascript "gameInit" gameInitJs :: Int -> IO ()
foreign export javascript "gameLoopBody" gameLoopBodyJS :: JSString -> IO JSString
foreign export javascript "gameLoopStop" gameLoopStopJS :: JSString -> IO Bool
foreign export javascript "configLoopBody" configLoopBodyJS :: JSString -> IO JSString
foreign export javascript "configLoopStop" configLoopStopJS :: JSString -> IO Bool
foreign export javascript "initialGame" initialGameJs :: JSString -> Int -> IO JSString

gameInitJs :: Int -> IO ()
gameInitJs seed = let interface = CJInterface in gameInit interface seed

gameLoopBodyJS :: JSString -> IO JSString
gameLoopBodyJS jsString =
    let interface = CJInterface
     in do
            args <- getGameLoopBodyArg jsString
            res <- gameLoopBody interface args
            return $ makeJsString res

gameLoopStopJS :: JSString -> IO Bool
gameLoopStopJS jsString = do
    args <- getGameLoopBodyArg jsString
    return $ gameLoopStop args

configLoopBodyJS :: JSString -> IO JSString
configLoopBodyJS jsString =
    let interface = CJInterface
     in do
            args <- getConfigLoopBodyArg jsString
            res <- configLoopBody interface args
            return $ makeJsString res

configLoopStopJS :: JSString -> IO Bool
configLoopStopJS jsString = do
    args <- getConfigLoopBodyArg jsString
    return $ configLoopStop args

initialGameJs :: JSString -> Int -> IO JSString
initialGameJs jsString seed = do
    config <- getInitialGameConfig jsString
    return $ makeJsString (initialGame config seed)

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
                    c_deinit
                    error $ "ERROR: Wrong game loop body arguments.\nArguments: " ++ jArgsString

getConfigLoopBodyArg :: JSString -> IO (Maybe GameConfig, Bool)
getConfigLoopBodyArg jArgs =
    let decoded = makeHsType jArgs
     in case decoded of
            Just args -> return args
            Nothing ->
                do
                    let jArgsString = WP.fromJSString jArgs
                    c_deinit
                    error $ "ERROR: Wrong config loop body arguments.\nArguments: " ++ jArgsString

getInitialGameConfig :: JSString -> IO GameConfig
getInitialGameConfig jsString =
    let decoded = makeHsType jsString
     in case decoded of
            Just gameConf -> return gameConf
            Nothing ->
                do
                    let gameConfigString = WP.fromJSString jsString
                    c_deinit
                    error $ "ERROR: Wrong game config.\nGame Config: " ++ gameConfigString

foreign import ccall "get_user_action" c_getUserAction :: IO CString
foreign import ccall "update_c_state_and_render_game" c_updateCStateAndRenderGame :: CString -> IO ()
foreign import ccall "update_draw_config" c_updateDrawConfig :: IO CString
foreign import ccall "init" c_init :: Int -> IO ()
foreign import ccall "deinit" c_deinit :: IO ()
foreign import ccall "new_round" c_newRound :: IO ()
foreign import ccall "game_should_stop" c_gameShouldStop :: IO Bool
foreign import ccall "should_game_restart" c_shouldGameRestart :: IO Bool

data CJInterface = CJInterface

instance Interface CJInterface where
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

makeHsTypeC :: (FromJSON a) => CString -> IO (Maybe a)
makeHsTypeC cs = do
    s <- CS.peekCAString cs
    return $ AS.decode $ BL.pack s

getGameConfig :: CString -> IO (Maybe GameConfig)
getGameConfig cGameConfig = do
    decoded <- makeHsTypeC cGameConfig
    case decoded of
        Just gameConf -> free cGameConfig >> return gameConf
        Nothing ->
            do
                cGameConfigString <- CS.peekCAString cGameConfig
                c_deinit
                error $ "ERROR: Wrong game config.\nGame Config: " ++ cGameConfigString

getCurrentUserAction :: CString -> IO (Maybe PlayerAction)
getCurrentUserAction cAction = do
    decoded <- makeHsTypeC cAction
    case decoded of
        Just actions -> return actions
        Nothing ->
            do
                cActionString <- CS.peekCAString cAction
                c_deinit
                error $ "ERROR: Wrong players action.\nPlayer Action: " ++ cActionString

-- NOTE: Needed such that it compiles
main :: IO ()
main = return ()
