module TestTichu (tichuTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map

import Game.Structures
import Game.Tichu
import MockGame

tichuTests :: TestTree
tichuTests =
    testGroup
        "Tichu Tests"
        [ dealXCards'Test
        , deal0CardsTest
        , deal1CardDealerFirstTest
        , deal1CardDealerLastTest
        ]

dealXCards'Test :: TestTree
dealXCards'Test =
    testCase "dealXCards'" $
        let dealedHands =
                dealXCards'
                    ["Alice", "Bob", "Charlie", "David"]
                    (take 5 testDeck)
                    (Map.fromList [("Alice", []), ("Bob", []), ("Charlie", []), ("David", [])])
         in assertEqual
                "Everybody has one card"
                ( Map.fromList
                    [
                        ( "Alice"
                        , [PokerCard (Two, Red)]
                        )
                    , ("Bob", [PokerCard (Three, Red)])
                    , ("Charlie", [PokerCard (Four, Red)])
                    , ("David", [PokerCard (Five, Red)])
                    ]
                , [PokerCard (Six, Red)]
                )
                dealedHands

deal0CardsTest :: TestTree
deal0CardsTest =
    testCase "deal0Cards" $
        let game = dealXCards (testGame "Alice") 0
         in assertEqual
                "Empty hands"
                ( Map.fromList
                    [ ("Alice", [])
                    , ("Bob", [])
                    , ("Charlie", [])
                    , ("David", [])
                    ]
                )
                (hands game)

deal1CardDealerFirstTest :: TestTree
deal1CardDealerFirstTest =
    testCase "deal1CardDealerFirst" $
        let game = dealXCards (testGame "Alice") 1
         in assertEqual
                "Everybody has one card"
                ( Map.fromList
                    [ ("Alice", [PokerCard (Five, Red)])
                    , ("Bob", [PokerCard (Two, Red)])
                    , ("Charlie", [PokerCard (Three, Red)])
                    , ("David", [PokerCard (Four, Red)])
                    ]
                )
                (hands game)

deal1CardDealerLastTest :: TestTree
deal1CardDealerLastTest =
    testCase "deal1CardDealerFirst" $
        let game = dealXCards (testGame "David") 1
         in assertEqual
                "Everybody has one card"
                ( Map.fromList
                    [ ("Alice", [PokerCard (Two, Red)])
                    , ("Bob", [PokerCard (Three, Red)])
                    , ("Charlie", [PokerCard (Four, Red)])
                    , ("David", [PokerCard (Five, Red)])
                    ]
                )
                (hands game)

-- TODO: Implement
-- allPossibleCombinationsTest :: TestTree
-- allPossibleCombinationsTest =
--     testCase "allPossibleCombinationsTest" $
--         assertEqual "PossibleCombinationsAreCorrect"
--             (possiblePlayerActions testGamePlay "Alice") []
