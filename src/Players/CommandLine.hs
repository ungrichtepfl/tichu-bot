module Players.CommandLine (commandLinePlayer, commandLinePlayer') where

import Control.Monad (foldM_)

import qualified Data.Map as Map

import Game.Structures
import Game.Utils
import Players.IO

commandLinePlayer :: Game -> [PlayerAction] -> PlayerName -> IO PlayerAction
commandLinePlayer game allPossibleActions pn = do
    showPlayerInfo
    putStrLnQI "Combination to beat:"
    putStrLnQI (showLastPlayedCardsSep " " game ++ " ") -- Space needed for unicode symbols to show correctly
    putStrLnQI "Full board:"
    printQI $ map cardsFromCombination (board game)
    putStrLnQI ("Hands of player " ++ show pn ++ ":")
    putStrLnQI (showListSep " " (hands game Map.! pn) ++ " ") -- Space needed for unicode symbols to show correctly
    askForPlayerAction pn allPossibleActions
  where
    showPlayerInfo :: IO ()
    showPlayerInfo =
        let currentPlayer = getCurrentPlayer game
         in if pn == currentPlayer
                then
                    putStrLnQ
                        ( "It is players "
                            ++ show currentPlayer
                            ++ " turn. What do you want to do?"
                        )
                else
                    putStrLnQ
                        ( "Does player "
                            ++ show pn
                            ++ " want to do something before player "
                            ++ show currentPlayer
                            ++ " does its turn?"
                        )

askForPlayerAction :: PlayerName -> [PlayerAction] -> IO PlayerAction
askForPlayerAction playerName possibleActions = do
    putStrLnQI "Possible actions:"
    foldM_ printWithNumber 0 possibleActions
    putStrLnQI "Enter the number of the desired action:"
    rawInput <- getTrimmedLine
    case rawInput of
        "" ->
            if Pass `elem` possibleActions
                then putStrLnA "Passing" >> return Pass
                else
                    putStrLnE "You are not allowed to pass!"
                        >> askForPlayerAction playerName possibleActions
        _ -> do
            maybeAction <- selectFromList possibleActions rawInput
            case maybeAction of
                Nothing -> askForPlayerAction playerName possibleActions
                Just Stop ->
                    putStrLnQ
                        (show playerName ++ " wanted to exit.")
                        >> exitGame
                Just action -> putStrLnA ("Player " ++ show playerName ++ " played: " ++ show action) >> return action

commandLinePlayer' :: (String, GamePlayer)
commandLinePlayer' = ("commandLinePlayer", commandLinePlayer)
