module Lib (
  someFunc,
) where

import Control.Monad.Random
import System.Random
import System.Random.TF

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum, Bounded)

data Color = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq, Enum, Bounded)

data TichuCard = PokerCard (Value, Color) | Dragon | Phoenix | Mahjong | Dog
  deriving (Show, Eq)

type TichuCards = [TichuCard]

type PlayerName = String
type TeamName = String

type Score = Int

data Player = Player
  { playerName :: PlayerName
  , hand :: TichuCards
  , trick :: TichuCards
  }
  deriving (Show, Eq)

data Team = Team
  { players :: (Player, Player)
  , teamName :: TeamName
  , score :: Score
  }
  deriving (Show, Eq)

data GameRound = Dealing Int | Distributing Int | Playing Int | Counting
  deriving (Show, Eq)

data Game = Game
  { teams :: (Team, Team)
  , round :: GameRound
  , deck :: TichuCards
  , scoreLimit :: Int
  }

-- Random: https://stackoverflow.com/questions/59619690/generate-a-random-list-of-custom-data-type-where-values-provided-to-data-constr
someFunc :: IO ()
someFunc = putStrLn "someFunc"
