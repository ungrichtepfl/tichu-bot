{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Control.Monad.Loops
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Maybe (isJust, isNothing)
import Foreign.C.String (CString)

import qualified Data.Aeson as AS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import qualified Foreign.C.String as CS

import Bots.Random
import Game.Interface
import Game.Structures
import Game.Tichu
import Game.Utils

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

userPlayerIndex :: Int
userPlayerIndex = 2

getUserPlayerName :: Game -> PlayerName
getUserPlayerName game = playerNames' game !! userPlayerIndex

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

getCurrentAction ::
    (Interface interface) =>
    interface -> Game -> Maybe (Map PlayerName [PlayerAction]) -> IO (Maybe (PlayerName, PlayerAction), Game)
getCurrentAction _ game Nothing = return (Nothing, game)
getCurrentAction interface game (Just playerActions) =
    case gamePhase game of
        Playing player _ _ ->
            if player == getUserPlayerName game
                then do
                    userAction <- getUserAction interface
                    return ((\a -> (player, a)) <$> userAction, game)
                else
                    let actions = playerActions Map.! player
                        (botAction, g) = randomPlayer game actions player
                     in return (Just (player, botAction), g)
        _ -> return (Nothing, game)

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

configLoopStop :: (Maybe GameConfig, Bool) -> Bool
configLoopStop configOutput = isJust (fst configOutput) || snd configOutput

configLoopBody :: (Interface interface) => interface -> (Maybe GameConfig, Bool) -> IO (Maybe GameConfig, Bool)
configLoopBody interface _ = do
    conf <- updateDrawConfig interface
    end <- gameShouldStop interface
    return (conf, end)

configLoop :: (Interface interface) => interface -> IO (Maybe GameConfig)
configLoop interface =
    do
        (config, _) <- iterateUntilM configLoopStop (configLoopBody interface) (Nothing, False)
        return config

gameLoopBody ::
    (Interface interface) =>
    interface ->
    (Game, Maybe (Map PlayerName [PlayerAction])) ->
    IO (Game, Maybe (Map PlayerName [PlayerAction]))
gameLoopBody interface toSend@(game, possibleActions) =
    let
        restartStop (g, _) = shouldGameStop g || gamePhase g == Starting
        restartLoopBody ::
            (Game, Maybe (Map PlayerName [PlayerAction])) ->
            IO (Game, Maybe (Map PlayerName [PlayerAction]))
        restartLoopBody (g, _) = do
            let toSend' = (g, Nothing)
            updateStateAndRenderGame interface toSend'
            end <- gameShouldStop interface
            restart <- shouldGameRestart interface
            if restart && not end
                then return (newGame (generator g) (gameConfig g), Nothing)
                else return (g{shouldGameStop = end}, Nothing)
     in
        do
            let currentPlayingPlayer = case gamePhase game of
                    Playing p _ _ -> p
                    _ -> ""
            updateStateAndRenderGame interface toSend
            (action, game') <- getCurrentAction interface game possibleActions
            case action of
                Just a -> putStrLn $ ">>> " ++ fst a ++ " played : " ++ show (snd a)
                _ -> return ()
            let (game'', possibleActions') =
                    if isNothing action && currentPlayingPlayer == getUserPlayerName game'
                        then (game', possibleActions)
                        else
                            updateGame game' action
            end <- gameShouldStop interface
            let game''' = game''{shouldGameStop = end}
            case gamePhase game''' of
                NextRound -> newRound interface >> return (game''', possibleActions')
                Finished -> iterateUntilM restartStop restartLoopBody (game''', Nothing)
                _ -> return (game''', possibleActions')

gameLoopStop :: (Game, Maybe (Map PlayerName [PlayerAction])) -> Bool
gameLoopStop (game, _) = shouldGameStop game

gameLoop :: (Interface interface) => interface -> GameConfig -> IO ()
gameLoop interface config =
    iterateUntilM gameLoopStop (gameLoopBody interface) (initialGame config 0) >> return ()

main :: IO ()
main =
    do
        let interface = CInterface
        gameInit interface userPlayerIndex
        mConfig <- configLoop interface
        case mConfig of
            Just config ->
                gameLoop interface config >> gameDeinit interface
            Nothing -> gameDeinit interface
