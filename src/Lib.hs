module Lib (
  playTichu,
) where

import Control.Monad
import Data.Array.IO
import System.Random

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum, Bounded)

data Color = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq, Enum)

data TichuCard = PokerCard (Value, Color) | Dragon | Phoenix | Mahjong | Dog
  deriving (Show, Eq)

type TichuCards = [TichuCard]

type PlayerName = String

type TeamName = String

type Score = Int

data GameConfig = GameConfig
  { sittingOrder :: [PlayerName]
  , teamNames :: [TeamName]
  , scoreLimit :: Score
  }
  deriving (Show, Eq)

playerNames :: GameConfig -> [PlayerName]
playerNames = sittingOrder

class Player player where
  play :: player -> Game -> IO TichuAction

newtype Distribution = Distribution [(PlayerName, [(PlayerName, TichuCard)])]
  deriving (Show, Eq)

data GameRound = Dealing PlayerName | Distributing Distribution | Playing [TichuAction] | Counting
  deriving (Show, Eq)

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray' n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
 where
  n = length xs
  newArray' :: Int -> [a] -> IO (IOArray Int a)
  newArray' n' = newListArray (1, n')

data TichuAction = Play TichuCards | Pass | Tichu | GrandTichu
  deriving (Show, Eq)

data Game = Game
  { gameConfig :: GameConfig
  , hands :: [(PlayerName, TichuCards)]
  , tricks :: [(PlayerName, TichuCards)]
  , tichuRound :: GameRound
  , tichus :: [(PlayerName, Bool)]
  , deck :: TichuCards
  }
  deriving (Show, Eq)

orderedDeck :: TichuCards
orderedDeck = [PokerCard (v, c) | v <- [Two .. Ace], c <- [Spades .. Clubs]] ++ [Dragon, Phoenix, Mahjong, Dog]

shuffledDeck :: IO TichuCards
shuffledDeck = shuffle orderedDeck

initialHands :: [PlayerName] -> [(PlayerName, TichuCards)]
initialHands names = [(n, []) | n <- names]

initialTricks :: [PlayerName] -> [(PlayerName, TichuCards)]
initialTricks names = [(n, []) | n <- names]

initialTichus :: [PlayerName] -> [(PlayerName, Bool)]
initialTichus names = [(n, False) | n <- names]

initialGame :: GameConfig -> IO Game
initialGame gameConfig = do
  initialDeck <- shuffledDeck
  shuffledPlayers <- shuffle $ playerNames gameConfig
  let randomPlayer = head shuffledPlayers
  let orderedPlayers = sittingOrder gameConfig
  return $
    Game
      { gameConfig = gameConfig
      , hands = initialHands orderedPlayers
      , tricks = initialTricks orderedPlayers
      , tichuRound = Dealing randomPlayer
      , tichus = initialTichus orderedPlayers
      , deck = initialDeck
      }

playTichu :: IO ()
playTichu = do
  game <- initialGame $ GameConfig ["Alice", "Bob", "Charlie", "David"] ["Team 1", "Team 2"] 1000
  print game
