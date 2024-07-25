module MockGame (testGame, testDeck) where

import qualified Data.Map        as Map
import           Game.Structures

testDeck :: TichuCards
testDeck = [PokerCard (v, c) | c <- [Spades .. Clubs], v <- [Two .. Ace]] ++ [Dragon, Phoenix, Mahjong, Dog]

testGame :: String -> Game
testGame dealer =
    Game
        { gameConfig = GameConfig ["Alice", "Bob", "Charlie", "David"] ["Team 1", "Team 2"] 1000
        , hands = Map.fromList [("Alice", []), ("Bob", []), ("Charlie", []), ("David", [])]
        , tricks = Map.fromList [("Alice", []), ("Bob", []), ("Charlie", []), ("David", [])]
        , gamePhase = Dealing testDeck
        , tichus = Map.fromList [("Alice", Nothing), ("Bob", Nothing), ("Charlie", Nothing), ("David", Nothing)]
        , scores = Map.fromList [("Team 1", 0), ("Team 2", 0)]
        , board = []
        , currentDealer = dealer
        , finishOrder = []
        , shouldGameStop = False
        }
