module Players.Random (randomPlayer, randomPlayer') where

import System.Random (randomRIO)

import Game.Structures

randomPlayer :: GamePlayer
randomPlayer _ allPossibleActions _ = do
    let possibleActions = filter (Stop /=) allPossibleActions
    if null possibleActions
        then error "No possible actions."
        else do
            index <- randomRIO (0, length possibleActions - 1)
            return $ possibleActions !! index

randomPlayer' :: (String, GamePlayer)
randomPlayer' = ("randomPlayer", randomPlayer)
