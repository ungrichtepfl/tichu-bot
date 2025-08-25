module Bots.Random (randomPlayer, randomPlayer') where

import System.Random (uniformShuffleList)

import Game.Structures

randomPlayer :: GamePlayer
randomPlayer game allPossibleActions _ =
    let possibleActions = filter (Stop /=) allPossibleActions
     in if null possibleActions
            then error "No possible actions."
            else
                let (actionShuffled, g) = uniformShuffleList possibleActions (generator game)
                    action = head actionShuffled
                 in (action, game{generator = g})

randomPlayer' :: (String, GamePlayer)
randomPlayer' = ("randomPlayer", randomPlayer)
