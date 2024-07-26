module Players.GamePlayer (module Players.GamePlayer) where

import Game.Structures
import Players.CommandLine
import Players.Random

type Name = String

defaultPlayerTypes :: [(Name, GamePlayer)]
defaultPlayerTypes = [commandLinePlayer', randomPlayer', randomPlayer', randomPlayer']

textToPlayer :: String -> Maybe GamePlayer
textToPlayer p
    | p == fst commandLinePlayer' = Just commandLinePlayer
    | p == fst randomPlayer' = Just randomPlayer
    | otherwise = Nothing
