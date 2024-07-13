module Players.Random (pickPlayerActionRandom) where

import Structures
import System.Random (randomRIO)

pickPlayerActionRandom :: Game -> [PlayerAction] -> PlayerName -> IO PlayerAction
pickPlayerActionRandom _ allPossibleActions _ = do
  let possibleActions = filter (Stop /=) allPossibleActions
  if null possibleActions
    then error "No possible actions."
    else do
      index <- randomRIO (0, length possibleActions - 1)
      return $ possibleActions !! index
