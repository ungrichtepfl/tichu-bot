module TestTichu (tichuTests) where

import qualified Data.Map         as Map
import           MockGame
import           Structures
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tichu

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
                , [PokerCard (Two, Spades)]
                )
              , ("Bob", [PokerCard (Three, Spades)])
              , ("Charlie", [PokerCard (Four, Spades)])
              , ("David", [PokerCard (Five, Spades)])
              ]
          , [PokerCard (Six, Spades)]
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
              [ ("Alice", [PokerCard (Five, Spades)])
              , ("Bob", [PokerCard (Two, Spades)])
              , ("Charlie", [PokerCard (Three, Spades)])
              , ("David", [PokerCard (Four, Spades)])
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
              [ ("Alice", [PokerCard (Two, Spades)])
              , ("Bob", [PokerCard (Three, Spades)])
              , ("Charlie", [PokerCard (Four, Spades)])
              , ("David", [PokerCard (Five, Spades)])
              ]
          )
          (hands game)
