module GamePlayer (module GamePlayer) where

import Players.CommandLine
import Players.Random
import Structures

data GamePlayer = CommandLinePlayer | RandomPlayer
  deriving (Show, Eq, Read)

class Playable a where
  pickPlayerAction :: a -> Game -> [PlayerAction] -> PlayerName -> IO PlayerAction

instance Playable GamePlayer where
  pickPlayerAction CommandLinePlayer = pickPlayerActionCommandLine
  pickPlayerAction RandomPlayer      = pickPlayerActionRandom
