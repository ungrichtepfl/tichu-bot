{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Structures (module Game.Structures) where

import Data.Aeson (FromJSON (..), ToJSON (..), withArray)
import Data.Aeson.Types (Parser)
import Data.Bits (xor)
import Data.Map (Map)
import Data.Scientific (toBoundedInteger)
import Data.Vector (toList)
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Random (StdGen, genWord64, mkStdGen)

import qualified Data.Aeson as Aeson

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

data Color = Red | Green | Blue | Black
    --          Herz | Kreuz | Eggen | Schaufel
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
    [ -- Herz

        [ '\x1F0B2'
        , '\x1F0B3'
        , '\x1F0B4'
        , '\x1F0B5'
        , '\x1F0B6'
        , '\x1F0B7'
        , '\x1F0B8'
        , '\x1F0B9'
        , '\x1F0BA'
        , '\x1F0BB'
        , '\x1F0BD'
        , '\x1F0BE'
        , '\x1F0B1'
        ]
    , -- Kreuz

        [ '\x1F0D2'
        , '\x1F0D3'
        , '\x1F0D4'
        , '\x1F0D5'
        , '\x1F0D6'
        , '\x1F0D7'
        , '\x1F0D8'
        , '\x1F0D9'
        , '\x1F0DA'
        , '\x1F0DB'
        , '\x1F0DD'
        , '\x1F0DE'
        , '\x1F0D1'
        ]
    , -- Eggen

        [ '\x1F0C2'
        , '\x1F0C3'
        , '\x1F0C4'
        , '\x1F0C5'
        , '\x1F0C6'
        , '\x1F0C7'
        , '\x1F0C8'
        , '\x1F0C9'
        , '\x1F0CA'
        , '\x1F0CB'
        , '\x1F0CD'
        , '\x1F0CE'
        , '\x1F0C1'
        ]
    , -- Schaufel

        [ '\x1F0A2'
        , '\x1F0A3'
        , '\x1F0A4'
        , '\x1F0A5'
        , '\x1F0A6'
        , '\x1F0A7'
        , '\x1F0A8'
        , '\x1F0A9'
        , '\x1F0AA'
        , '\x1F0AB'
        , '\x1F0AD'
        , '\x1F0AE'
        , '\x1F0A1'
        ]
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

type PlayerToBeat = Maybe PlayerName

data GamePhase
    = Starting
    | Dealing TichuCards
    | Distributing
    | Playing PlayerName Passes PlayerToBeat
    | GiveAwayLooserTricksAndHands
    | Scoring
    | NextRound
    | Finished
    deriving (Eq)

instance Show GamePhase where
    show Starting = "Starting"
    show (Dealing _) = "Dealing "
    show Distributing = "Distributing"
    show (Playing pn passes playerToBeat) = "Players turn: " ++ show pn ++ ". Passes before: " ++ show passes ++ "Player to beat: " ++ show playerToBeat
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
    , winnerTeams :: [TeamName]
    , shouldGameStop :: Bool
    , generator :: StdGen
    }
    deriving (Show, Eq)

type GamePlayer = Game -> [PlayerAction] -> PlayerName -> IO PlayerAction

type GamePlayers = [GamePlayer]

-- JSON SERIALIZATION / DESERIALIZATION

deriving instance Generic Value
deriving instance Generic Color
deriving instance Generic TichuCard
deriving instance Generic TichuCombination
deriving instance Generic GameConfig
deriving instance Generic GamePhase
deriving instance Generic TichuType
deriving instance Generic PlayerAction
deriving instance Generic Game

instance ToJSON StdGen where
    toJSON g = Aeson.toJSON (take 2 (genWords g))
      where
        genWords :: StdGen -> [Word64]
        genWords gen0 =
            let (w1, gen1) = genWord64 gen0
                (w2, _) = genWord64 gen1
             in [w1, w2]

instance FromJSON StdGen where
    parseJSON = withArray "StdGen" $ \arr -> do
        case toList arr of
            [v1, v2] -> do
                w1 <- parseWord64 v1
                w2 <- parseWord64 v2
                pure (mkStdGen (fromIntegral (w1 `xor` w2)))
            _ -> fail "Expected array of two elements"
      where
        parseWord64 :: Aeson.Value -> Parser Word64
        parseWord64 v = do
            n <- Aeson.parseJSON v
            maybe (fail "Invalid Word64") pure (toBoundedInteger n)

instance FromJSON Value
instance ToJSON Value

instance FromJSON Color
instance ToJSON Color

instance FromJSON TichuCard
instance ToJSON TichuCard

instance FromJSON TichuCombination
instance ToJSON TichuCombination

instance FromJSON GameConfig
instance ToJSON GameConfig

instance FromJSON GamePhase
instance ToJSON GamePhase

instance FromJSON TichuType
instance ToJSON TichuType

instance FromJSON PlayerAction
instance ToJSON PlayerAction

instance FromJSON Game
instance ToJSON Game
