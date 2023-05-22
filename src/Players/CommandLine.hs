module Players.CommandLine (pickPlayerActionCommandLine) where

import           Control.Monad (foldM_)
import qualified Data.Map      as Map
import           IO
import           Utils

import           Structures

pickPlayerActionCommandLine :: Game -> [PlayerAction] -> PlayerName -> IO PlayerAction
pickPlayerActionCommandLine game allPossibleActions pn = do
  showPlayerInfo
  putStrLnQI "Combination to beat:"
  putStrLnQI $ showLastPlayedCards game
  putStrLnQI "Full board:"
  printQI $ map cardsFromCombination (board game)
  putStrLnQI ("Hands of player " ++ show pn ++ ":")
  putStrLnQI $ showList' (hands game Map.! pn)
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
