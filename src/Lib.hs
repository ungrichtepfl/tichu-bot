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

data ComboType = Single | Pair | Tripple | Straight5 | Straight6 | Straight7 | Straight8
    | Straight9 | Straight10 | Straight11 | Straight12 | Straight13 | Straight14 | FullHouse
    | Stair2 | Stair3 | Stair4 | Stair5 | Stair6 | Stair7 | LittleBomb | BigBomb5 | BigBomb6
    | BigBomb7 | BigBomb8 | BigBomb9 | BigBomb10 | BigBomb11 | BigBomb12 | BigBomb13 | Dog
  deriving (Show, Eq)

data Combo = Combo
  { comboType :: ComboType
  , level :: Int --should equal TWICE the Value uf the highest card involved, so that the phoenix can fit in between
  , cards :: TichuCards
  }

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
