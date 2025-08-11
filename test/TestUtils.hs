module TestUtils (utilsTests) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map

import Game.Structures
import Game.Tichu
import Game.Utils
import MockGame

utilsTests :: TestTree
utilsTests =
    testGroup
        "Utils Tests"
        [ cardsScoreTest1
        , cardsScoreTest2
        , cardsScoreTest3
        , cardsScoreTest4
        , cardsScoreTest5
        , cardsScoreTest6
        , cardsScoreTest7
        , cardsScoreTest8
        ]

cardsScoreTest1 :: TestTree
cardsScoreTest1 =
    testCase "Score Cards 1" $
        assertEqual
            "No cards"
            0
            (cardsScore [])

cardsScoreTest2 :: TestTree
cardsScoreTest2 =
    testCase "Score Cards 2" $
        assertEqual
            "Only Dragon"
            25
            (cardsScore [Dragon])

cardsScoreTest3 :: TestTree
cardsScoreTest3 =
    testCase "Score Cards 3" $
        assertEqual
            "Only Phoenix"
            (-25)
            (cardsScore [Phoenix])

cardsScoreTest4 :: TestTree
cardsScoreTest4 =
    testCase "Score Cards 4" $
        assertEqual
            "Phoenix and Dragon"
            0
            (cardsScore [Phoenix, Dragon])

cardsScoreTest5 :: TestTree
cardsScoreTest5 =
    testCase "Score Cards 5" $
        assertEqual
            "Full deck"
            100
            (cardsScore orderedDeck)

cardsScoreTest6 :: TestTree
cardsScoreTest6 =
    testCase "Score Cards 6" $
        assertEqual
            "King"
            10
            (cardsScore [PokerCard (King, Blue)])

cardsScoreTest7 :: TestTree
cardsScoreTest7 =
    testCase "Score Cards 7" $
        assertEqual
            "Ten"
            10
            (cardsScore [PokerCard (Ten, Black)])

cardsScoreTest8 :: TestTree
cardsScoreTest8 =
    testCase "Score Cards 8" $
        assertEqual
            "Five"
            5
            (cardsScore [PokerCard (Five, Red)])
