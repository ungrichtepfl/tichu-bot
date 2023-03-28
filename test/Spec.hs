{-# LANGUAGE ImportQualifiedPost #-}

import Data.Map qualified as Map
import Lib
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain
    ( testGroup
        "Tichu Tests"
        [ dealXCards'Test
        , deal0CardsTest
        , deal1CardDealerFirstTest
        , deal1CardDealerLastTest
        , testIsNOfAKind
        , testIsNOfAKind'
        , testIsNotNOfAKind
        , testIsNotNOfAKind'
        , testIsNotNOfAKind''
        , testIsNotNOfAKind'''
        ]
    )

testDeck :: TichuCards
testDeck = [PokerCard (v, c) | c <- [Spades .. Clubs], v <- [Two .. Ace]] ++ [Dragon, Phoenix, Mahjong, Dog]

testGame :: String -> Game
testGame dealer =
  Game
    { gameConfig = GameConfig ["Alice", "Bob", "Charlie", "David"] ["Team 1", "Team 2"] 1000
    , hands = Map.fromList [("Alice", []), ("Bob", []), ("Charlie", []), ("David", [])]
    , tricks = Map.fromList [("Alice", []), ("Bob", []), ("Charlie", []), ("David", [])]
    , tichuRound = Dealing testDeck
    , tichus = Map.fromList [("Alice", False), ("Bob", False), ("Charlie", False), ("David", False)]
    , scores = Map.fromList [("Team 1", 0), ("Team 2", 0)]
    , currentDealer = dealer
    }

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

testIsNOfAKind :: TestTree
testIsNOfAKind =
  testCase "isNOfAKind" $
    let cards =
          [ PokerCard (Two, Spades)
          , PokerCard (Two, Hearts)
          , PokerCard (Two, Diamonds)
          , PokerCard (Two, Clubs)
          ]
     in assertEqual "Four of a kind" True (isNOfAKind 4 cards)

testIsNOfAKind' :: TestTree
testIsNOfAKind' =
  testCase "isNOfAKind" $
    let cards =
          [ PokerCard (Two, Spades)
          , Phoenix
          , PokerCard (Two, Diamonds)
          , PokerCard (Two, Clubs)
          ]
     in assertEqual "Four of a kind" True (isNOfAKind 4 cards)

testIsNotNOfAKind :: TestTree
testIsNotNOfAKind =
  testCase "isNotNOfAKind" $
    let cards =
          [ PokerCard (Two, Spades)
          , PokerCard (Three, Hearts)
          , PokerCard (Two, Diamonds)
          , PokerCard (Two, Clubs)
          ]
     in assertEqual "Not four of a kind" False (isNOfAKind 4 cards)
testIsNotNOfAKind' :: TestTree
testIsNotNOfAKind' =
  testCase "isNotNOfAKind'" $
    let cards =
          [ PokerCard (Two, Spades)
          , PokerCard (Two, Diamonds)
          , PokerCard (Two, Clubs)
          ]
     in assertEqual "Not four of a kind" False (isNOfAKind 4 cards)

testIsNotNOfAKind'' :: TestTree
testIsNotNOfAKind'' =
  testCase "isNotNOfAKind''" $
    let cards =
          [ PokerCard (Two, Spades)
          , PokerCard (Two, Hearts)
          , PokerCard (Two, Diamonds)
          , PokerCard (Two, Clubs)
          , PokerCard (Three, Diamonds)
          ]
     in assertEqual "Not four of a kind" False (isNOfAKind 4 cards)

testIsNotNOfAKind''' :: TestTree
testIsNotNOfAKind''' =
  testCase "isNotNOfAKind'''" $
    let cards =
          [ PokerCard (Two, Spades)
          , PokerCard (Two, Hearts)
          , PokerCard (Two, Diamonds)
          , PokerCard (Two, Clubs)
          , Phoenix
          ]
     in assertEqual "Not four of a kind" False (isNOfAKind 4 cards)
