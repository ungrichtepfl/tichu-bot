module CommandLinePlayer (module CommandLinePlayer) where

import Control.Monad (foldM_)

import qualified Data.Map as Map

import Bots.Random
import Game.Structures
import Game.Utils
import IO

commandLinePlayer :: Game -> [PlayerAction] -> PlayerName -> IO (PlayerAction, Game)
commandLinePlayer game allPossibleActions pn = do
    showPlayerInfo
    putStrLnQI "Combination to beat:"
    putStrLnQI (showLastPlayedCardsSep " " game ++ " ") -- Space needed for unicode symbols to show correctly
    putStrLnQI "Full board:"
    printQI $ map cardsFromCombination (board game)
    putStrLnQI ("Hands of player " ++ show pn ++ ":")
    putStrLnQI (showListSep " " (hands game Map.! pn) ++ " ") -- Space needed for unicode symbols to show correctly
    action <- askForPlayerAction pn allPossibleActions
    return (action, game)
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
    putStrLnQI "Enter the number of the desired action (just press enter to pass):"
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

commandLinePlayer' :: (String, GamePlayerIO)
commandLinePlayer' = ("commandLinePlayer", commandLinePlayer)

randomPlayerIO :: GamePlayerIO
randomPlayerIO a b c = return $ randomPlayer a b c

randomPlayerIO' :: (String, GamePlayerIO)
randomPlayerIO' = (fst randomPlayer', randomPlayerIO)

defaultPlayerTypes :: [(String, GamePlayerIO)]
defaultPlayerTypes = [commandLinePlayer', randomPlayerIO', randomPlayerIO', randomPlayerIO']

textToPlayer :: String -> Maybe GamePlayerIO
textToPlayer p
    | p == fst commandLinePlayer' = Just commandLinePlayer
    | p == fst randomPlayer' = Just randomPlayerIO
    | otherwise = Nothing
