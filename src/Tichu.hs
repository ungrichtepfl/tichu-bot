{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module Tichu where

import Control.Monad (forM)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Data.List (elemIndex, foldl', nub, nubBy, sort, tails, (\\))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import System.Random (randomRIO)
import Text.Read (readMaybe)

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
  deriving (Show, Eq, Ord, Enum, Bounded, Read)

data Color = Spades | Hearts | Diamonds | Clubs
  deriving (Show, Eq, Enum, Read)

data TichuCard = PokerCard (Value, Color) | Dragon | Phoenix | Mahjong | Dog
  deriving (Show, Eq, Read)

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
isNOfAKind 1 [_] = True
isNOfAKind nb cards
  | containsSpecialCardsNoPhoenix cards = False
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

isSigleCard :: TichuCards -> Bool
isSigleCard = isNOfAKind 1

isPair :: TichuCards -> Bool
isPair = isNOfAKind 2

isThreeOfAKind :: TichuCards -> Bool
isThreeOfAKind = isNOfAKind 3

isFourOfAKind :: TichuCards -> Bool
isFourOfAKind = isNOfAKind 4

hasSameColor :: TichuCards -> Bool
hasSameColor cards =
  let colors = map color cards
   in noNothings colors && length (nub colors) == 1

isStraight :: TichuCards -> Bool
isStraight tichuCards = any (isNstraight tichuCards) [5, 6, 7, 8, 9, 10, 11, 12, 13, 14]

isNstraight :: TichuCards -> Int -> Bool
isNstraight cards n
  | length cards /= n = False
  | Mahjong `elem` cards && Just Two `elem` map value cards = isStraight' $ filter (/= Mahjong) cards
  | otherwise = isStraight' cards
 where
  isStraight' cards'
    | containsSpecialCardsNoPhoenix cards' = False
    | otherwise =
        let values = mapMaybe value cards'
            minVal = minimum values
            maxVal = maximum values
            values' = [minVal .. maxVal]
            values'' = values' \\ values
         in length values'' <= 1 && nub values == values

isBomb :: TichuCards -> Bool
isBomb cards
  | containsSpecialCards cards = False
  | otherwise = isFourOfAKind cards || isSameColorStraight
 where
  isSameColorStraight = hasSameColor cards && isStraight cards

drawKfromN :: [a] -> Int -> [[a]]
drawKfromN _ 0 = [[]]
drawKfromN xs n =
  [y : ys | y : xs' <- tails xs, ys <- drawKfromN xs' (n - 1)]

isFullHouse :: TichuCards -> Bool
isFullHouse cards
  | length cards /= 5 = False
  | containsSpecialCardsNoPhoenix cards = False
  | otherwise =
      let triples = drawKfromN (sort cards) 3
       in any (\triple -> isThreeOfAKind triple && isPair (cards \\ triple)) triples

isStairs :: TichuCards -> Bool
isStairs cards
  | length cards < 4 = False
  | containsSpecialCardsNoPhoenix cards = False
  | otherwise =
      let uniqueValues = nubBy (\c1 c2 -> value c1 == value c2) (nonPhoenixCards cards)
       in length cards == 2 * length uniqueValues && isNstraight uniqueValues (length uniqueValues)

type TichuCards = [TichuCard]

type PlayerName = String

type TeamName = String

type Score = Int

data TichuCombination
  = SingleCard TichuCards
  | Pair TichuCards Value
  | ThreeOfAKind TichuCards Value
  | Straight TichuCards Value
  | FullHouse TichuCards Value
  | Stairs TichuCards Value
  | Bomb TichuCards Value
  deriving (Show, Eq, Read)

cardsFromCombination :: TichuCombination -> TichuCards
cardsFromCombination (SingleCard cards) = cards
cardsFromCombination (Pair cards _) = cards
cardsFromCombination (ThreeOfAKind cards _) = cards
cardsFromCombination (Straight cards _) = cards
cardsFromCombination (FullHouse cards _) = cards
cardsFromCombination (Stairs cards _) = cards
cardsFromCombination (Bomb cards _) = cards

possibleCombinations :: TichuCards -> [TichuCombination]
possibleCombinations cards = concatMap combinationsLengthK [1 .. length cards]
 where
  combinationsLengthK :: Int -> [TichuCombination]
  combinationsLengthK k =
    let combinations = drawKfromN cards k
     in mapMaybe toTichuCombination combinations

toTichuCombination :: TichuCards -> Maybe TichuCombination
toTichuCombination cards
  | isSigleCard cards = Just $ SingleCard sortedCards
  | isPair cards = Just $ Pair sortedCards maximumValue
  | isFullHouse cards = Just $ FullHouse sortedCards maximumValueFullHouse
  | isStraight cards = Just $ Straight sortedCards maximumValueStraight
  | isBomb cards = Just $ Bomb sortedCards maximumValue
  | isStairs cards = Just $ Stairs sortedCards maximumValue
  | isThreeOfAKind cards = Just $ ThreeOfAKind sortedCards maximumValue
  | otherwise = Nothing
 where
  sortedCards = sort cards
  maximumValue :: Value
  maximumValue = maximum $ mapMaybe value cards
  maximumValueStraight :: Value
  maximumValueStraight
    | Phoenix `notElem` cards = maximumValue
    | otherwise -- FIXME: Phoenix as the lowerst card not possible
      =
        let cardsNonPhoenix = nonPhoenixCards cards
         in if isStraight cardsNonPhoenix then succ maximumValue else maximumValue
  maximumValueFullHouse :: Value
  maximumValueFullHouse =
    let triple = filter isThreeOfAKind (drawKfromN cards 3)
     in maximum $ mapMaybe ((value . head) . filter (/= Phoenix)) triple

data GameConfig = GameConfig
  { sittingOrder :: [PlayerName]
  , teamNames :: [TeamName]
  , scoreLimit :: Score
  }
  deriving (Show, Eq)

type Distribution = Map PlayerName (Map PlayerName TichuCard)

nextInOrder :: Game -> PlayerName -> PlayerName
nextInOrder game pn =
  let pns = sittingOrder $ gameConfig game
   in pns !! (fromJust (elemIndex pn pns) + 1 `mod` length pns)

emptyDistribution :: [PlayerName] -> Distribution
emptyDistribution pns = Map.fromList [(pn, Map.empty) | pn <- pns]

data GamePhase
  = Starting
  | Dealing TichuCards
  | Distributing
  | Playing PlayerName
  | Scoring
  | NextRound
  | Finished
  deriving (Show, Eq)

data PlayerAction = Start | Distribute (Map PlayerName TichuCard) | Play TichuCombination | Pass | Tichu | GrandTichu | Stop
  deriving (Show, Eq, Read)

data Game = Game
  { gameConfig :: GameConfig
  , hands :: Map PlayerName TichuCards
  , tricks :: Map PlayerName TichuCards
  , board :: [TichuCards]
  , gamePhase :: GamePhase
  , tichus :: Map PlayerName Bool
  , scores :: Map TeamName Score
  , currentDealer :: PlayerName
  , stop :: Bool
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

newGame :: GameConfig -> Game
newGame config =
  Game
    { gameConfig = config
    , hands = initialHands $ playerNames config
    , tricks = initialTricks $ playerNames config
    , board = []
    , gamePhase = Starting
    , tichus = initialTichus $ playerNames config
    , scores = initialScores $ teamNames config
    , currentDealer = head $ playerNames config
    , stop = False
    }

startGame :: Game -> IO Game
startGame game = do
  initialDeck <- shuffledDeck
  shuffledPlayers <- shuffle $ playerNames' game
  let randomPlayer = head shuffledPlayers
  return $
    game
      { gamePhase = Dealing initialDeck
      , currentDealer = randomPlayer
      }

nextRound :: Game -> IO Game
nextRound game = do
  initialDeck <- shuffledDeck
  return $
    game
      { hands = setEmpty $ hands game
      , tricks = setEmpty $ tricks game
      , gamePhase = Dealing initialDeck
      , tichus = setFalse $ tichus game
      , currentDealer = newDealer
      }
 where
  newDealer =
    let playerList = playerNames' game
     in playerList !! nextDealerIndex game

currentDealerIndex :: Game -> Int
currentDealerIndex game =
  fromJust $ elemIndex (currentDealer game) (playerNames' game) -- TODO: handle Nothing

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
  foldl' (\(hands', deck') pn -> dealCard pn deck' hands') (playerHands, deck) pns

dealXCards :: Game -> Int -> Game
dealXCards game nb = case gamePhase game of
  Dealing deck ->
    let (hands', deck') = dealXCards' dealPlayerOrder deck (hands game)
     in game{hands = hands', gamePhase = Dealing deck'}
  r -> error $ "Wrong round: " ++ show r
 where
  dealPlayerOrder =
    let playerList = playerNames' game
        i = currentDealerIndex game
        reorderedPlayerList = drop (i + 1) playerList ++ take (i + 1) playerList
     in concat $ replicate nb reorderedPlayerList

dealAllCards :: Game -> Game
dealAllCards game = case gamePhase game of
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
     in game'{gamePhase = Distributing}
  r -> error $ "Wrong round: " ++ show r

-- From https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
  | p v = return v
  | otherwise = f v >>= iterateUntilM p f

getGameConfig :: IO GameConfig
getGameConfig = do
  players <- getPlayers
  teamNames <- getTeamNames
  GameConfig players teamNames <$> getMaxScore

getMaxScore :: IO Int
getMaxScore = do
  putStrLn "Enter max score:"
  userInput <- readMaybe <$> getLine
  case userInput of
    Just x -> return x
    Nothing -> putStrLn "Wrong input should be a positive number." >> getMaxScore

getTeamNames :: IO [TeamName]
getTeamNames = do
  putStrLn "Enter team names, separated by spaces:"
  teamNames <- words <$> getLine
  if length teamNames == 2
    then return teamNames
    else putStrLn "Wrong number of teams, should be 2." >> getTeamNames

getPlayers :: IO [PlayerName]
getPlayers = do
  putStrLn "Enter player names, separated by spaces:"
  userInput <- words <$> getLine
  if length userInput == 4
    then return userInput
    else putStrLn "Wrong number of players, should be 4." >> getPlayers

possiblePlayerActions :: Game -> PlayerName -> [PlayerAction]
possiblePlayerActions game pn =
  let defaultActions = [Stop, Pass]
   in case gamePhase game of
        Playing playerPlaying ->
          let combinations = possibleCombinations $ hands game Map.! pn
           in defaultActions
                ++ if pn == playerPlaying
                  then map Play combinations
                  else
                    map Play $
                      filter
                        ( \case
                            Bomb _ _ -> True
                            _ -> False
                        )
                        combinations
        Dealing _ -> defaultActions
        Distributing -> defaultActions
        _ -> []

askForPlayerAction :: [PlayerAction] -> IO PlayerAction
askForPlayerAction possibleActions = do
  putStrLn "Possible actions:"
  -- TODO: Let player chose by number instead of name
  mapM_ print possibleActions
  putStrLn "Enter action:"
  action <- (\x -> if x == "" then Pass else read x) <$> getLine
  if action `elem` possibleActions
    then return action
    else
      putStrLn "Invalid action"
        >> askForPlayerAction possibleActions

getPlayerActions :: Game -> PlayerName -> IO (Map PlayerName PlayerAction)
getPlayerActions game playerPlaying = do
  let otherPlayers = filter (/= playerPlaying) $ playerNames' game
  playerAction <- askForPlayerAction $ possiblePlayerActions game playerPlaying
  otherPlayerActions <- mapM (askForPlayerAction . possiblePlayerActions game) otherPlayers
  return $ Map.fromList $ (playerPlaying, playerAction) : zip otherPlayers otherPlayerActions

update :: Game -> IO Game
update game =
  case gamePhase game of
    Starting -> startGame game
    Dealing _ -> return $ dealAllCards game
    Distributing -> distribute game
    Playing playerPlaying -> play game playerPlaying
    NextRound -> nextRound game
    Scoring -> return $ score game
    Finished -> finish game

applyPlayerAction :: Game -> PlayerName -> PlayerAction -> Game
applyPlayerAction game playerPlaying playerAction =
  -- TODO: Finish implementation
  case playerAction of
    Play combination ->
      let playerHand = hands game Map.! playerPlaying
          cards = cardsFromCombination combination
          newPlayerHand = playerHand \\ cards
          newHands = Map.insert playerPlaying newPlayerHand $ hands game
       in game{hands = newHands, board = cards : board game}
    Stop -> game{stop = True}
    _ -> game

play :: Game -> PlayerName -> IO Game
play game playerPlaying = do
  -- TODO: Finish implementation
  playerActions <- getPlayerActions game playerPlaying
  let newPlayerPlaying = nextInOrder game playerPlaying
  let game' = foldl' (\g (pn, pa) -> applyPlayerAction g pn pa) game $ Map.toList playerActions
  return game'{gamePhase = Playing newPlayerPlaying}

distribute :: Game -> IO Game
distribute game = return game{gamePhase = Playing $ startingPlayer $ hands game} -- TODO: Implement

startingPlayer :: Map PlayerName TichuCards -> PlayerName
startingPlayer = head . Map.keys . Map.filter (elem Mahjong)

display :: Game -> IO ()
display = print -- TODO: Implement

score :: Game -> Game
score game = game -- TODO: Implement

finish :: Game -> IO Game
finish game = return game -- TODO: Implement

run :: Game -> IO Game
run game = display game >> update game

playTichu :: IO ()
playTichu = getGameConfig >>= return . newGame >>= iterateUntilM stop run >>= display
