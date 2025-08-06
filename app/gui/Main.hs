{-# LANGUAGE ForeignFunctionInterface #-}

module Main (main) where

import Control.Monad.Loops
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as AS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import Foreign.C.String (CString)
import qualified Foreign.C.String as CS
import Game.Structures
import Game.Tichu
import Game.Utils
import Bots.Random

foreign import ccall "update_draw_game" c_updateDrawGame :: CString -> IO CString
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

getCurrentAction :: (Game , Maybe (Map PlayerName [PlayerAction])) -> IO (Maybe (PlayerName, PlayerAction))
getCurrentAction gameOutput@(game, mPlayerActions) = case mPlayerActions of
                                                Nothing -> return $ Nothing
                                                Just playerActions -> getCurrentAction' playerActions (playerNames' game)
    where
        getCurrentAction' _ [] = return $ Nothing
        getCurrentAction' playersAction (player:ps) = do
                                        action <- getActionForPlayer playersAction player
                                        if isNothing action then
                                            getCurrentAction' playersAction ps
                                            else return $ (\a -> (player, a)) <$> action

        getActionForPlayer playerActions player = if player == getUserPlayerName game then
                                            do
                                                cAction <- withCAString gameOutput c_updateDrawGame
                                                getCurrentUserAction cAction
                                    else case gamePhase game of
                                        Playing p _ -> if p == player then
                                                let actions = playerActions Map.! player in
                                                if null actions then return $ Nothing
                                                    else
                                                Just <$> randomPlayer game actions player
                                                else return $ Nothing
                                        _ -> return $ Nothing

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
        gameLoop gameOutput@(game, possibleActions) =
             do
                    action <- getCurrentAction gameOutput
                    putStrLn $ showLastPlayedCards game
                    let gameOutput' = updateGame game action
                    let game' = fst gameOutput'
                    end <- c_windowShouldClose
                    let game'' = game'{shouldGameStop = end || shouldGameStop game}
                    return (game'', snd gameOutput')
     in
        do
            c_init userPlayerIndex
            -- (mConfig, _) <- iterateUntilM shouldStopConfigLoop configLoop (Nothing, False)
            -- case mConfig of
            case Just ( GameConfig ["Alice", "Bob", "Charlie", "David"] ["Team 1", "Team 2"] 1000  ) of
                Just config ->
                    iterateUntilM shouldStopGameLoop gameLoop (newGame config 0) >> c_deinit
                Nothing -> c_deinit
