module Game.Structures (module Game.Structures) where

import Data.Map (Map)

type Passes = Int

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
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

data Color = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq, Enum, Read)

data TichuCard = PokerCard (Value, Color) | Dragon | Phoenix | Mahjong | Dog
  deriving (Eq, Read)

cardValue :: TichuCard -> Int
cardValue (PokerCard (King, _)) = 10
cardValue (PokerCard (Ten, _)) = 10
cardValue (PokerCard (Five, _)) = 5
cardValue Dragon = 25
cardValue Phoenix = -25
cardValue _ = 0

instance Show TichuCard where
  show (PokerCard (v, c)) = [pokerCardUnicodeMatrix !! fromEnum c !! fromEnum v]
  show Dragon = "Dragon"
  show Phoenix = "Phoenix"
  show Mahjong = "Mahjong"
  show Dog = "Dog"

pokerCardUnicodeMatrix :: [[Char]]
pokerCardUnicodeMatrix =
  [ ['\x1F0A2', '\x1F0A3', '\x1F0A4', '\x1F0A5', '\x1F0A6', '\x1F0A7', '\x1F0A8', '\x1F0A9', '\x1F0AA', '\x1F0AB', '\x1F0AD', '\x1F0AE', '\x1F0A1']
  , ['\x1F0B2', '\x1F0B3', '\x1F0B4', '\x1F0B5', '\x1F0B6', '\x1F0B7', '\x1F0B8', '\x1F0B9', '\x1F0BA', '\x1F0BB', '\x1F0BD', '\x1F0BE', '\x1F0B1']
  , ['\x1F0C2', '\x1F0C3', '\x1F0C4', '\x1F0C5', '\x1F0C6', '\x1F0C7', '\x1F0C8', '\x1F0C9', '\x1F0CA', '\x1F0CB', '\x1F0CD', '\x1F0CE', '\x1F0C1']
  , ['\x1F0D2', '\x1F0D3', '\x1F0D4', '\x1F0D5', '\x1F0D6', '\x1F0D7', '\x1F0D8', '\x1F0D9', '\x1F0DA', '\x1F0DB', '\x1F0DD', '\x1F0DE', '\x1F0D1']
  ]

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
  compare _ Dog = GT
  compare Mahjong _ = LT
  compare _ Mahjong = GT

color :: TichuCard -> Maybe Color
color (PokerCard (_, c)) = Just c
color _ = Nothing

value :: TichuCard -> Maybe Value
value (PokerCard (v, _)) = Just v
value _ = Nothing

type TichuCards = [TichuCard]

type PlayerName = String

type TeamName = String

type Score = Int

type Amount = Int

data TichuCombination
  = SingleCard TichuCards
  | Pair TichuCards Value
  | ThreeOfAKind TichuCards Value
  | Straight TichuCards Value
  | FullHouse TichuCards Value
  | Stairs TichuCards Value
  | Bomb TichuCards Value
  deriving (Show, Eq, Read)

data GameConfig = GameConfig
  { sittingOrder :: [PlayerName]
  , teamNames :: [TeamName]
  , scoreLimit :: Score
  }
  deriving (Show, Eq)

type Distribution = Map PlayerName (Map PlayerName TichuCard)

data GamePhase
  = Starting
  | Dealing TichuCards
  | Distributing
  | Playing PlayerName Passes
  | GiveAwayLooserTricksAndHands
  | Scoring
  | NextRound
  | Finished
  deriving (Eq)

instance Show GamePhase where
  show Starting = "Starting"
  show (Dealing _) = "Dealing "
  show Distributing = "Distributing"
  show (Playing pn passes) = "Players turn: " ++ show pn ++ ". Passes before: " ++ show passes
  show GiveAwayLooserTricksAndHands = "GiveAwayLooserTricksAndHands"
  show Scoring = "Scoring"
  show NextRound = "NextRound"
  show Finished = "Finished"

data TichuType = Tichu | GrandTichu
  deriving (Show, Eq)

data PlayerAction
  = Pass
  | Play TichuCombination
  | CallTichu
  | CallGrandTichu
  | Stop
  deriving (Show, Eq, Read)

data Game = Game
  { gameConfig :: GameConfig
  , hands :: Map PlayerName TichuCards
  , tricks :: Map PlayerName TichuCards
  , board :: [TichuCombination]
  , gamePhase :: GamePhase
  , tichus :: Map PlayerName (Maybe TichuType)
  , scores :: Map TeamName Score
  , currentDealer :: PlayerName
  , finishOrder :: [PlayerName]
  , shouldGameStop :: Bool
  }
  deriving (Show, Eq)

type GamePlayer = Game -> [PlayerAction] -> PlayerName -> IO PlayerAction

type GamePlayers = [GamePlayer]
