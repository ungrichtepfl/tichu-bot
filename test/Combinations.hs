import qualified Data.Map         as Map
import           Test.Tasty
import           Test.Tasty.HUnit
import           Tichu

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
            , testStraight
            , testStraight1
            , testStraight2
            , testStraight3
            , testStraight4
            , testStraight5
            , testStraight6
            , testStraight7
            , testStraight8
            , testStraight9
            , testStraight10
            , testStraight11
            , testStraight12
            , testStraight13
            , testBomb
            , testBomb1
            , testBomb2
            , testBomb3
            , testBomb4
            , testBomb5
            , testBomb6
            , testBomb7
            , testFullHouse
            , testFullHouse1
            , testFullHouse2
            , testFullHouse3
            , testFullHouse4
            , testFullHouse5
            , testStairs
            , testStairs1
            , testStairs2
            , testStairs3
            , testStairs4
            , testStairs5
            ]
        )

testDeck :: TichuCards
testDeck = [PokerCard (v, c) | c <- [Spades .. Clubs], v <- [Two .. Ace]] ++ [Dragon, Phoenix, Mahjong, Dog]

testGame :: String -> Game
testGame dealer =
    Game
        { gameConfig = GameConfig ["Alice", "Bob", "Charlie", "David"] [False, False, False, False] ["Team 1", "Team 2"] 1000
        , hands = Map.fromList [("Alice", []), ("Bob", []), ("Charlie", []), ("David", [])]
        , tricks = Map.fromList [("Alice", []), ("Bob", []), ("Charlie", []), ("David", [])]
        , gamePhase = Dealing testDeck
        , tichus = Map.fromList [("Alice", Nothing), ("Bob", Nothing), ("Charlie", Nothing), ("David", Nothing)]
        , scores = Map.fromList [("Team 1", 0), ("Team 2", 0)]
        , board = []
        , currentDealer = dealer
        , gamePlayers = Map.fromList [("Alice", CLI CLIPlayer), ("Bob", CLI CLIPlayer), ("Charlie", CLI CLIPlayer), ("David", CLI CLIPlayer)]
        , finishOrder = []
        , shouldGameStop = False
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

testStraight :: TestTree
testStraight =
    testCase "isStraight 4 cards" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Three, Hearts)
                , PokerCard (Four, Diamonds)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" False (isStraight cards)

testStraight1 :: TestTree
testStraight1 =
    testCase "isStraight 6 cards true" $
        let cards =
                [ PokerCard (Six, Clubs)
                , PokerCard (Two, Spades)
                , PokerCard (Four, Diamonds)
                , PokerCard (Three, Hearts)
                , PokerCard (Seven, Clubs)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight2 :: TestTree
testStraight2 =
    testCase "isStraight 5 cards false" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Six, Clubs)
                , PokerCard (Three, Hearts)
                , PokerCard (Seven, Clubs)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight3 :: TestTree
testStraight3 =
    testCase "isStraight 5 cards Phoenix in middle true" $
        let cards =
                [ PokerCard (Three, Hearts)
                , PokerCard (Two, Spades)
                , Phoenix
                , PokerCard (Six, Clubs)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight4 :: TestTree
testStraight4 =
    testCase "isStraight 5 cards Phoenix starts with two true" $
        let cards =
                [ Phoenix
                , PokerCard (Two, Spades)
                , PokerCard (Four, Clubs)
                , PokerCard (Three, Hearts)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight5 :: TestTree
testStraight5 =
    testCase "isStraight 5 cards Phoenix in beginning/end true" $
        let cards =
                [ Phoenix
                , PokerCard (Six, Clubs)
                , PokerCard (Three, Hearts)
                , PokerCard (Five, Clubs)
                , PokerCard (Four, Spades)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight6 :: TestTree
testStraight6 =
    testCase "isStraight 6 cards Phoenix, end is Ace, true" $
        let cards =
                [ PokerCard (Ten, Spades)
                , PokerCard (King, Clubs)
                , PokerCard (Jack, Hearts)
                , Phoenix
                , PokerCard (Queen, Clubs)
                , PokerCard (Ace, Clubs)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight7 :: TestTree
testStraight7 =
    testCase "isStraight 6 cards Phoenix in middle, end is Ace, false" $
        let cards =
                [ PokerCard (Nine, Spades)
                , PokerCard (King, Clubs)
                , PokerCard (Jack, Hearts)
                , Phoenix
                , PokerCard (Queen, Clubs)
                , PokerCard (Ace, Clubs)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight8 :: TestTree
testStraight8 =
    testCase "isStraight 6 cards Phoenix false" $
        let cards =
                [ PokerCard (Nine, Spades)
                , PokerCard (Seven, Clubs)
                , PokerCard (Jack, Hearts)
                , Phoenix
                , PokerCard (Queen, Clubs)
                , PokerCard (Ace, Clubs)
                ]
         in assertEqual "Straight" False (isStraight cards)

testStraight9 :: TestTree
testStraight9 =
    testCase "isStraight 5 cards Mahjong true" $
        let cards =
                [ PokerCard (Two, Spades)
                , Mahjong
                , PokerCard (Three, Hearts)
                , PokerCard (Four, Clubs)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight10 :: TestTree
testStraight10 =
    testCase "isStraight 5 cards Mahjong false" $
        let cards =
                [ Mahjong
                , PokerCard (Two, Spades)
                , PokerCard (Four, Clubs)
                , PokerCard (Nine, Hearts)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" False (isStraight cards)

testStraight11 :: TestTree
testStraight11 =
    testCase "isStraight 5 cards Special cards false" $
        let cards =
                [ PokerCard (Two, Spades)
                , Dragon
                , PokerCard (Three, Hearts)
                , PokerCard (Four, Clubs)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" False (isStraight cards)

testStraight12 :: TestTree
testStraight12 =
    testCase "isStraight 5 cards Mahjong and Phoenix true" $
        let cards =
                [ Mahjong
                , PokerCard (Two, Spades)
                , PokerCard (Four, Clubs)
                , Phoenix
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Straight" True (isStraight cards)

testStraight13 :: TestTree
testStraight13 =
    testCase "isStraight 6 cards Mahjong and Phoenix and Dragon False" $
        let cards =
                [ Mahjong
                , PokerCard (Two, Spades)
                , PokerCard (Four, Clubs)
                , Phoenix
                , PokerCard (Five, Clubs)
                , Dragon
                ]
         in assertEqual "Straight" False (isStraight cards)

testBomb :: TestTree
testBomb =
    testCase "isBomb 4 cards true" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Two, Hearts)
                , PokerCard (Two, Diamonds)
                , PokerCard (Two, Clubs)
                ]
         in assertEqual "Bomb" True (isBomb cards)

testBomb1 :: TestTree
testBomb1 =
    testCase "isBomb 4 cards False" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Two, Hearts)
                , PokerCard (Three, Diamonds)
                , PokerCard (Two, Clubs)
                ]
         in assertEqual "Bomb" False (isBomb cards)

testBomb2 :: TestTree
testBomb2 =
    testCase "isBomb 5 cards straight true" $
        let cards =
                [ PokerCard (Two, Clubs)
                , PokerCard (Six, Clubs)
                , PokerCard (Four, Clubs)
                , PokerCard (Three, Clubs)
                , PokerCard (Five, Clubs)
                ]
         in assertEqual "Bomb" True (isBomb cards)

testBomb3 :: TestTree
testBomb3 =
    testCase "isBomb 5 cards straight wrong number false" $
        let cards =
                [ PokerCard (Two, Clubs)
                , PokerCard (Eight, Clubs)
                , PokerCard (Four, Clubs)
                , PokerCard (Five, Clubs)
                , PokerCard (Six, Clubs)
                ]
         in assertEqual "Bomb" False (isBomb cards)

testBomb4 :: TestTree
testBomb4 =
    testCase "isBomb 5 cards straight Phoenix false" $
        let cards =
                [ PokerCard (Two, Clubs)
                , PokerCard (Four, Clubs)
                , PokerCard (Five, Clubs)
                , Phoenix
                , PokerCard (Six, Clubs)
                ]
         in assertEqual "Bomb" False (isBomb cards)

testBomb5 :: TestTree
testBomb5 =
    testCase "isBomb 4 cards Phoenix false" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Two, Diamonds)
                , Phoenix
                , PokerCard (Two, Clubs)
                ]
         in assertEqual "Bomb" False (isBomb cards)

testBomb6 :: TestTree
testBomb6 =
    testCase "isBomb 5 cards straight Dragon false" $
        let cards =
                [ PokerCard (Two, Clubs)
                , Dragon
                , PokerCard (Four, Clubs)
                , PokerCard (Five, Clubs)
                , PokerCard (Three, Clubs)
                ]
         in assertEqual "Bomb" False (isBomb cards)

testBomb7 :: TestTree
testBomb7 =
    testCase "isBomb 5 cards straight wrong color false" $
        let cards =
                [ PokerCard (Two, Clubs)
                , PokerCard (Six, Spades)
                , PokerCard (Four, Clubs)
                , PokerCard (Five, Clubs)
                , PokerCard (Three, Clubs)
                ]
         in assertEqual "Bomb" False (isBomb cards)

testFullHouse :: TestTree
testFullHouse =
    testCase "isFullHouse 5 cards true" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Two, Hearts)
                , PokerCard (Two, Diamonds)
                , PokerCard (Three, Clubs)
                , PokerCard (Three, Hearts)
                ]
         in assertEqual "FullHouse" True (isFullHouse cards)

testFullHouse1 :: TestTree
testFullHouse1 =
    testCase "isFullHouse 5 cards false" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Three, Clubs)
                , PokerCard (Two, Diamonds)
                , PokerCard (Two, Hearts)
                , Dragon
                ]
         in assertEqual "FullHouse" False (isFullHouse cards)

testFullHouse2 :: TestTree
testFullHouse2 =
    testCase "isFullHouse 5 cards Phoenix true" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Three, Clubs)
                , PokerCard (Two, Hearts)
                , Phoenix
                , PokerCard (Three, Hearts)
                ]
         in assertEqual "FullHouse" True (isFullHouse cards)

testFullHouse3 :: TestTree
testFullHouse3 =
    testCase "isFullHouse 4 cards false" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Three, Clubs)
                , PokerCard (Two, Hearts)
                , PokerCard (Four, Hearts)
                ]
         in assertEqual "FullHouse" False (isFullHouse cards)

testFullHouse4 :: TestTree
testFullHouse4 =
    testCase "isFullHouse 6 cards false" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Two, Hearts)
                , PokerCard (Two, Diamonds)
                , PokerCard (Three, Clubs)
                , PokerCard (Three, Hearts)
                , PokerCard (Three, Diamonds)
                ]
         in assertEqual "FullHouse" False (isFullHouse cards)

testFullHouse5 :: TestTree
testFullHouse5 =
    testCase "isFullHouse 5 cards Phoenix false" $
        let cards =
                [ Dog
                , PokerCard (Three, Clubs)
                , PokerCard (Four, Hearts)
                , Phoenix
                , PokerCard (Three, Hearts)
                ]
         in assertEqual "FullHouse" False (isFullHouse cards)

testStairs :: TestTree
testStairs =
    testCase "isStairs 4 cards true" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Three, Diamonds)
                , PokerCard (Two, Hearts)
                , PokerCard (Three, Clubs)
                ]
         in assertEqual "Stairs" True (isStairs cards)

testStairs1 :: TestTree
testStairs1 =
    testCase "isStairs 4 cards Phoenix true" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Three, Clubs)
                , Phoenix
                , PokerCard (Two, Hearts)
                ]
         in assertEqual "Stairs" True (isStairs cards)

testStairs2 :: TestTree
testStairs2 =
    testCase "isStairs 3 cards false" $
        let cards =
                [ PokerCard (Two, Spades)
                , Phoenix
                , PokerCard (Two, Hearts)
                ]
         in assertEqual "Stairs" False (isStairs cards)

testStairs3 :: TestTree
testStairs3 =
    testCase "isStairs 5 cards false" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Three, Diamonds)
                , PokerCard (Four, Clubs)
                , PokerCard (Two, Hearts)
                , PokerCard (Three, Clubs)
                ]
         in assertEqual "Stairs" False (isStairs cards)

testStairs4 :: TestTree
testStairs4 =
    testCase "isStairs 6 cards true" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Four, Clubs)
                , PokerCard (Three, Diamonds)
                , PokerCard (Two, Hearts)
                , PokerCard (Three, Clubs)
                , PokerCard (Four, Hearts)
                ]
         in assertEqual "Stairs" True (isStairs cards)

testStairs5 :: TestTree
testStairs5 =
    testCase "isStairs 6 cards false" $
        let cards =
                [ PokerCard (Two, Spades)
                , PokerCard (Three, Diamonds)
                , PokerCard (Two, Hearts)
                , PokerCard (Five, Clubs)
                , PokerCard (Three, Clubs)
                , PokerCard (Five, Hearts)
                ]
         in assertEqual "Stairs" True (isStairs cards)
