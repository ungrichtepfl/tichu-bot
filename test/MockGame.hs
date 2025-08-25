module MockGame (testGame, testDeck, testGamePlay) where

import System.Random (mkStdGen)

import qualified Data.Map as Map

import Game.Structures

testDeck :: TichuCards
testDeck = [PokerCard (v, c) | c <- [Red .. Black], v <- [Two .. Ace]] ++ [Dragon, Phoenix, Mahjong, Dog]

testGame :: String -> Game
testGame dealer =
    let gen = mkStdGen 0
     in Game
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
            , winnerTeams = []
            , generator = gen
            }

-- "Alice" -> [Dog,🂲,🂳,🃖,🃆,🂪,🃋,🂭,🃍,🂾,🃞,🃎,🃑,Dragon]
-- "Bob" -> [Mahjong,🃂,🂤,🂴,🂥,🂧,🃇,🃗,🂨,🂸,🂹,🂫,🂽,🂱]
-- "Charlie" -> [🃓,🃃,🃄,🃔,🃕,🂦,🂶,🂷,🃙,🃊,🃚,🂻,🃝,🂮]
-- "David" -> [🂢,🃒,🂣,🃅,🂵,🃘,🃈,🃉,🂩,🂺,🃛,🃁,🂡,Phoenix]
testGamePlay :: Game
testGamePlay =
    Game
        { gameConfig = GameConfig ["Alice", "Bob", "Charlie", "David"] ["Team 1", "Team 2"] 1000
        , hands =
            Map.fromList
                [
                    ( "Alice"
                    ,
                        [ Dog
                        , PokerCard (Two, Red)
                        , PokerCard (Three, Red)
                        , PokerCard (Six, Green)
                        , PokerCard (Six, Blue)
                        , PokerCard (Ten, Black)
                        , PokerCard (Jack, Blue)
                        , PokerCard (Queen, Black)
                        , PokerCard (Queen, Blue)
                        , PokerCard (King, Red)
                        , PokerCard (King, Green)
                        , PokerCard (King, Blue)
                        , PokerCard (Ace, Green)
                        , Dragon
                        ]
                    )
                ,
                    ( "Bob"
                    ,
                        [ Mahjong
                        , PokerCard (Two, Blue)
                        , PokerCard (Four, Black)
                        , PokerCard (Four, Red)
                        , PokerCard (Five, Black)
                        , PokerCard (Seven, Black)
                        , PokerCard (Seven, Blue)
                        , PokerCard (Seven, Green)
                        , PokerCard (Eight, Black)
                        , PokerCard (Eight, Red)
                        , PokerCard (Nine, Red)
                        , PokerCard (Jack, Black)
                        , PokerCard (Queen, Red)
                        , PokerCard (Ace, Red)
                        ]
                    )
                ,
                    ( "Charlie"
                    ,
                        [ PokerCard (Three, Green)
                        , PokerCard (Three, Blue)
                        , PokerCard (Four, Blue)
                        , PokerCard (Four, Green)
                        , PokerCard (Five, Green)
                        , PokerCard (Six, Black)
                        , PokerCard (Six, Red)
                        , PokerCard (Seven, Red)
                        , PokerCard (Nine, Green)
                        , PokerCard (Ten, Blue)
                        , PokerCard (Ten, Green)
                        , PokerCard (Jack, Red)
                        , PokerCard (Queen, Green)
                        , PokerCard (King, Black)
                        ]
                    )
                ,
                    ( "David"
                    ,
                        [ PokerCard (Two, Black)
                        , PokerCard (Two, Green)
                        , PokerCard (Three, Black)
                        , PokerCard (Five, Blue)
                        , PokerCard (Five, Red)
                        , PokerCard (Eight, Green)
                        , PokerCard (Eight, Blue)
                        , PokerCard (Nine, Blue)
                        , PokerCard (Nine, Black)
                        , PokerCard (Ten, Red)
                        , PokerCard (Jack, Green)
                        , PokerCard (Ace, Blue)
                        , PokerCard (Ace, Black)
                        , Phoenix
                        ]
                    )
                ]
        , tricks = Map.fromList [("Alice", []), ("Bob", []), ("Charlie", []), ("David", [])]
        , gamePhase = Playing "Alice" 0 Nothing
        , tichus = Map.fromList [("Alice", Nothing), ("Bob", Nothing), ("Charlie", Nothing), ("David", Nothing)]
        , scores = Map.fromList [("Team 1", 0), ("Team 2", 0)]
        , board = []
        , currentDealer = "Alice"
        , finishOrder = []
        , shouldGameStop = False
        , winnerTeams = []
        , generator = mkStdGen 0
        }
