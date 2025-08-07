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
import Game.Structures
import Game.Tichu
import Game.Utils

foreign import ccall "get_user_action" c_getUserAction :: IO CString
foreign import ccall "update_c_state_and_render_game" c_updateCStateAndRenderGame :: CString -> IO ()
foreign import ccall "update_draw_config" c_updateDrawConfig :: IO CString
foreign import ccall "init" c_init :: Int -> IO ()
foreign import ccall "deinit" c_deinit :: IO ()
foreign import ccall "window_should_close" c_windowShouldClose :: IO Bool

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

getCurrentAction :: Game -> Maybe (Map PlayerName [PlayerAction]) -> IO (Maybe (PlayerName, PlayerAction))
getCurrentAction _ Nothing = return Nothing
getCurrentAction game (Just playerActions) =
    case gamePhase game of
        Playing player _ ->
            if player == getUserPlayerName game
                then do
                    cAction <- c_getUserAction
                    userAction <- getCurrentUserAction cAction
                    return $ (\a -> (player, a)) <$> userAction
                else
                    let actions = playerActions Map.! player
                     in do
                            botAction <- randomPlayer game actions player
                            return $ Just (player, botAction)
        _ -> return Nothing

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

showListSep :: (Show a) => String -> [a] -> String
showListSep _ [] = ""
showListSep _ [x] = show x
showListSep sep (x : xs) = show x ++ sep ++ showListSep sep xs

showLastPlayedCardsSep :: String -> Game -> String
showLastPlayedCardsSep sep game = case board game of
    [] -> "Empty board"
    (lastComb : _) -> showListSep sep $ cardsFromCombination lastComb

showLastPlayedCards :: Game -> String
showLastPlayedCards = showLastPlayedCardsSep ", "

configLoop :: IO (Maybe GameConfig)
configLoop =
    let stop configOutput = isJust (fst configOutput) || snd configOutput
        loop _ = do
            cConf <- c_updateDrawConfig
            mConf <- getGameConfig cConf
            end <- c_windowShouldClose
            return (mConf, end)
     in do
            (mConfig, _) <- iterateUntilM stop loop (Nothing, False)
            return mConfig

gameLoop :: GameConfig -> IO ()
gameLoop config =
    let
        stop (game, _) = shouldGameStop game
        loop ::
            (Game, Maybe (Map PlayerName [PlayerAction])) ->
            IO (Game, Maybe (Map PlayerName [PlayerAction]))
        loop toSend@(game, possibleActions) = do
            let currentPlayingPlayer = case gamePhase game of
                    Playing p _ -> p
                    _ -> ""
            withCAString toSend c_updateCStateAndRenderGame
            action <- getCurrentAction game possibleActions
            let (game', possibleActions') =
                    if isNothing action && currentPlayingPlayer == getUserPlayerName game
                        then (game, possibleActions)
                        else updateGame game action
            end <- c_windowShouldClose
            let game'' = game'{shouldGameStop = end || shouldGameStop game'}
            return (game'', possibleActions')
     in
        do
            iterateUntilM stop loop (newGame config 0) >> c_deinit

main :: IO ()
main =
    do
        c_init userPlayerIndex
        -- mConfig <- configLoop
        -- case mConfig of
        case Just (GameConfig ["Alice", "Bob", "Charlie", "David"] ["Team 1", "Team 2"] 1000) of
            Just config ->
                gameLoop config >> c_deinit
            Nothing -> c_deinit
