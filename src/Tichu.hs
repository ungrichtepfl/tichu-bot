{-# LANGUAGE ImportQualifiedPost #-}

module Tichu where

import Control.Monad (forM)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Data.List (elemIndex, nub, (\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import System.Random (randomRIO)

setEmpty :: Map k [a] -> Map k [a]
setEmpty = Map.map (const [])

setFalse :: Map k Bool -> Map k Bool
setFalse = Map.map (const False)

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

data Value
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum, Bounded)

data Color = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq, Enum)

data TichuCard = PokerCard (Value, Color) | Dragon | Phoenix | Mahjong | Dog
  deriving (Show, Eq)

instance Ord TichuCard where
  compare (PokerCard (v1, _)) (PokerCard (v2, _)) = compare v1 v2
  compare Dragon Dragon = EQ
  compare Phoenix Phoenix = EQ
  compare Mahjong Mahjong = EQ
  compare Dog Dog = EQ
  compare Dragon _ = GT
  compare _ Dragon = LT
  compare Phoenix _ = GT
  compare _ Phoenix = LT
  compare Dog _ = LT -- Dog needs to be before Mahjong, is lower than any card
  compare _ Dog = LT
  compare Mahjong _ = LT
  compare _ Mahjong = LT

color :: TichuCard -> Maybe Color
color (PokerCard (_, c)) = Just c
color _ = Nothing

value :: TichuCard -> Maybe Value
value (PokerCard (v, _)) = Just v
value _ = Nothing

nonPhoenixCards :: TichuCards -> TichuCards
nonPhoenixCards = filter (/= Phoenix)

containsSpecialCards :: TichuCards -> Bool
containsSpecialCards cards = any (`elem` cards) [Dragon, Phoenix, Mahjong, Dog]

containsSpecialCardsNoPhoenix :: TichuCards -> Bool
containsSpecialCardsNoPhoenix cards = any (`elem` cards) [Dragon, Mahjong, Dog]

noNothings :: Eq a => [Maybe a] -> Bool
noNothings = all (`notElem` Nothing)

isNOfAKind :: Int -> TichuCards -> Bool
isNOfAKind _ [Phoenix] = False
isNOfAKind nb cards
  | Phoenix `elem` cards =
      let cards' = nonPhoenixCards cards
       in check cards' (nb - 1)
  | otherwise = check cards nb
 where
  check [] _ = False
  check cards'' n
    | length cards'' /= n = False
    | otherwise =
        let values = map value cards''
         in noNothings values && length (filter (== head values) values) == n

isThreeOfAKind :: TichuCards -> Bool
isThreeOfAKind = isNOfAKind 3

isPair :: TichuCards -> Bool
isPair = isNOfAKind 2

hasSameColor :: TichuCards -> Bool
hasSameColor cards =
  let colors = map color cards
   in noNothings colors && length (nub colors) == 1

isStraight :: TichuCards -> Bool
isStraight cards
  | length cards < 5 = False
  | Mahjong `elem` cards && Just Two `elem` map value cards = isStraight' $ filter (/= Mahjong) cards
  | containsSpecialCardsNoPhoenix cards = False
  | otherwise = isStraight' cards
 where
  isStraight' cards' =
    let cardsNonPhoenix = nonPhoenixCards cards'
        values = map (fromJust . value) cardsNonPhoenix
        minVal = minimum values
        maxVal = maximum values
        values' = [minVal .. maxVal]
        values'' = values' \\ values
     in length values'' <= 1

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

class Playable p where
  play :: p -> IO PlayerAction

class Distributable d where
  distribute :: d -> IO (Map PlayerName d)

type Distribution = Map PlayerName (Map PlayerName TichuCard)

emptyDistribution :: [PlayerName] -> Distribution
emptyDistribution pns = Map.fromList [(pn, Map.empty) | pn <- pns]

data GameRound
  = Dealing TichuCards
  | Distributing Distribution
  | Playing [PlayerAction]
  | Counting
  deriving (Show, Eq)

data PlayerAction = Play TichuCards | Pass | Tichu | GrandTichu
  deriving (Show, Eq)

data Game = Game
  { gameConfig :: GameConfig
  , hands :: Map PlayerName TichuCards
  , tricks :: Map PlayerName TichuCards
  , tichuRound :: GameRound
  , tichus :: Map PlayerName Bool
  , scores :: Map TeamName Score
  , currentDealer :: PlayerName
  }
  deriving (Show, Eq)

playerNames :: GameConfig -> [PlayerName]
playerNames = sittingOrder

playerNames' :: Game -> [PlayerName]
playerNames' = sittingOrder . gameConfig

orderedDeck :: TichuCards
orderedDeck =
  [ PokerCard (v, c) | v <- [Two .. Ace], c <- [Spades .. Clubs]
  ]
    ++ [Dragon, Phoenix, Mahjong, Dog]

shuffledDeck :: IO TichuCards
shuffledDeck = shuffle orderedDeck

initialHands :: [PlayerName] -> Map PlayerName TichuCards
initialHands names = Map.fromList [(n, []) | n <- names]

initialTricks :: [PlayerName] -> Map PlayerName TichuCards
initialTricks names = Map.fromList [(n, []) | n <- names]

initialTichus :: [PlayerName] -> Map PlayerName Bool
initialTichus names = Map.fromList [(n, False) | n <- names]

initialScores :: [TeamName] -> Map TeamName Score
initialScores names = Map.fromList [(n, 0) | n <- names]

newGame :: GameConfig -> IO Game
newGame config = do
  initialDeck <- shuffledDeck
  shuffledPlayers <- shuffle $ playerNames config
  let randomPlayer = head shuffledPlayers
  return $
    Game
      { gameConfig = config
      , hands = initialHands $ playerNames config
      , tricks = initialTricks $ playerNames config
      , tichuRound = Dealing initialDeck
      , tichus = initialTichus $ playerNames config
      , scores = initialScores $ teamNames config
      , currentDealer = randomPlayer
      }

resetGame :: Game -> IO Game
resetGame game = do
  initialDeck <- shuffledDeck
  return $
    game
      { hands = setEmpty $ hands game
      , tricks = setEmpty $ tricks game
      , tichuRound = Dealing initialDeck
      , tichus = setFalse $ tichus game
      , currentDealer = newDealer
      }
 where
  newDealer =
    let playerList = playerNames' game
     in playerList !! nextDealerIndex game

currentDealerIndex :: Game -> Int
currentDealerIndex game =
  fromJust $ elemIndex (currentDealer game) (playerNames' game)

nextDealerIndex :: Game -> Int
nextDealerIndex game =
  if currentDealerIndex game == length (playerNames' game) - 1
    then 0
    else currentDealerIndex game + 1

dealCard ::
  PlayerName ->
  TichuCards ->
  Map PlayerName TichuCards ->
  (Map PlayerName TichuCards, TichuCards)
dealCard pn deck playerHands =
  ( Map.insert pn (head deck : playerHands Map.! pn) playerHands
  , tail deck
  )

dealXCards' ::
  [PlayerName] ->
  TichuCards ->
  Map PlayerName TichuCards ->
  (Map PlayerName TichuCards, TichuCards)
dealXCards' pns deck playerHands =
  foldl (\(hands', deck') pn -> dealCard pn deck' hands') (playerHands, deck) pns

dealXCards :: Game -> Int -> Game
dealXCards game nb = case tichuRound game of
  Dealing deck ->
    let (hands', deck') = dealXCards' dealPlayerOrder deck (hands game)
     in game{hands = hands', tichuRound = Dealing deck'}
  r -> error $ "Wrong round: " ++ show r
 where
  dealPlayerOrder =
    let playerList = playerNames' game
        i = currentDealerIndex game
        reorderedPlayerList = drop (i + 1) playerList ++ take (i + 1) playerList
     in concat $ replicate nb reorderedPlayerList

dealAllCards :: Game -> Game
dealAllCards game = case tichuRound game of
  Dealing deck ->
    let nb =
          if length deck `mod` length (playerNames' game) == 0
            then length deck `div` length (playerNames' game)
            else
              error $
                "Wrong number of cards in deck: "
                  ++ show (length deck)
                  ++ ". There are "
                  ++ show (length (playerNames' game))
                  ++ " players in game."
        game' = dealXCards game nb
     in game'{tichuRound = Distributing $ emptyDistribution (playerNames' game')}
  r -> error $ "Wrong round: " ++ show r

-- TODO: implement real game logic
playTichu :: IO ()
playTichu = do
  game <- newGame $ GameConfig ["Alice", "Bob", "Charlie", "David"] ["Team 1", "Team 2"] 1000
  print game
  putStrLn "--------------------------------------------"
  let game' = dealAllCards game
  print game'
  putStrLn "--------------------------------------------"
  game'' <- resetGame game'
  putStrLn "--------------------------------------------"
  print game''
  putStrLn "--------------------------------------------"
