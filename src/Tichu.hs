{-# LANGUAGE LambdaCase #-}

-- Useful debug infos:
-- Enabling assertions: https://stackoverflow.com/questions/45777703/turning-on-assertions-while-compiling-with-haskells-stack-build-system
--  i.e. stack clean && stack build --fast && stack run
-- Profil build: https://stackoverflow.com/questions/32123475/profiling-builds-with-stack
--  i.e. stack run --profile -- +RTS -xc

module Tichu (module Tichu) where

import           Control.Exception (assert)
import           Control.Monad     (foldM, foldM_)
import           Data.Char         (isSpace)
import           Data.List         (elemIndex, foldl', nub, nubBy, sort, sortBy,
                                    tails, (\\))
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust, isJust, isNothing, mapMaybe)
import           System.Exit       (exitSuccess)
import           System.IO         (hFlush, stdout)
import           System.Random     (randomRIO)
import           Text.Read         (readMaybe)
import           Utils

----- CONSTANTS -------

quitSymbol :: String
quitSymbol = "q"

defaultPlayerNames :: [PlayerName]
defaultPlayerNames = ["P1", "P2", "P3", "P4"]

defaultTeamNames :: [TeamName]
defaultTeamNames = ["Team 1", "Team 2"]

defaultScoreLimit :: Int
defaultScoreLimit = 1000

maxCards :: Int
maxCards = 14

-----------------------

outputPromptChar :: Char
outputPromptChar = '>'

inputPromptChar :: Char
inputPromptChar = '$'

inputPrompt :: String
inputPrompt = inputPromptChar : " "

outputPrompt :: Int -> String
outputPrompt n = replicate n outputPromptChar ++ " "

putStrLnQ :: String -> IO ()
putStrLnQ = putStrLn . (outputPrompt 2 ++)

putStrLnA :: String -> IO ()
putStrLnA = putStrLn . (outputPrompt 1 ++)

putStrLnQI :: String -> IO ()
putStrLnQI = putStrLn . (outputPrompt 4 ++)

putStrLnE :: String -> IO ()
putStrLnE = putStrLn . ("ERROR: " ++)

putStrLnD :: String -> IO ()
putStrLnD = putStrLn . ("DEBUG: " ++)

printQ :: Show a => a -> IO ()
printQ = putStrLnQ . show

printA :: Show a => a -> IO ()
printA = putStrLnA . show

printQI :: Show a => a -> IO ()
printQI = putStrLnQI . show

printE :: Show a => a -> IO ()
printE = putStrLnE . show

trim :: String -> String
trim =
  let
    f = reverse . dropWhile isSpace
   in
    f . f

printPrompt :: IO ()
printPrompt = putStr inputPrompt

getLineFlushed :: IO String
getLineFlushed = hFlush stdout >> getLine

getTrimmedLine :: IO String
getTrimmedLine = printPrompt >> (trim <$> getLineFlushed)

printWithNumber :: Show a => Int -> a -> IO Int
printWithNumber i x = putStrLnQI ("(" ++ show i ++ ") " ++ show x) >> return (i + 1)

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
cardValue (PokerCard (Ten, _))  = 10
cardValue (PokerCard (Five, _)) = 5
cardValue Dragon                = 25
cardValue Phoenix               = -25
cardValue _                     = 0

cardsScore :: [TichuCard] -> Int
cardsScore = sum . map cardValue

instance Show TichuCard where
  show (PokerCard (v, c)) = [pokerCardUnicodeMatrix !! fromEnum c !! fromEnum v]
  show Dragon             = "Dragon"
  show Phoenix            = "Phoenix"
  show Mahjong            = "Mahjong"
  show Dog                = "Dog"

pokerCardUnicodeMatrix :: [[Char]]
pokerCardUnicodeMatrix =
  [ ['\x1F0A2', '\x1F0A3', '\x1F0A4', '\x1F0A5', '\x1F0A6', '\x1F0A7', '\x1F0A8', '\x1F0A9', '\x1F0AA', '\x1F0AB', '\x1F0AD', '\x1F0AE', '\x1F0A1']
  , ['\x1F0B2', '\x1F0B3', '\x1F0B4', '\x1F0B5', '\x1F0B6', '\x1F0B7', '\x1F0B8', '\x1F0B9', '\x1F0BA', '\x1F0BB', '\x1F0BD', '\x1F0BE', '\x1F0B1']
  , ['\x1F0C2', '\x1F0C3', '\x1F0C4', '\x1F0C5', '\x1F0C6', '\x1F0C7', '\x1F0C8', '\x1F0C9', '\x1F0CA', '\x1F0CB', '\x1F0CD', '\x1F0CE', '\x1F0C1']
  , ['\x1F0D2', '\x1F0D3', '\x1F0D4', '\x1F0D5', '\x1F0D6', '\x1F0D7', '\x1F0D8', '\x1F0D9', '\x1F0DA', '\x1F0DB', '\x1F0DD', '\x1F0DE', '\x1F0D1']
  ]

instance Ord TichuCard where
  compare (PokerCard (v1, _)) (PokerCard (v2, _)) = compare v1 v2
  compare Dragon Dragon                           = EQ
  compare Phoenix Phoenix                         = EQ
  compare Mahjong Mahjong                         = EQ
  compare Dog Dog                                 = EQ
  compare Dragon _                                = GT
  compare _ Dragon                                = LT
  compare Phoenix _                               = GT
  compare _ Phoenix                               = LT
  compare Dog _                                   = LT -- Dog needs to be before Mahjong, is lower than any card
  compare _ Dog                                   = GT
  compare Mahjong _                               = LT
  compare _ Mahjong                               = GT

color :: TichuCard -> Maybe Color
color (PokerCard (_, c)) = Just c
color _                  = Nothing

value :: TichuCard -> Maybe Value
value (PokerCard (v, _)) = Just v
value _                  = Nothing

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

cardsFromCombination :: TichuCombination -> TichuCards
cardsFromCombination (SingleCard cards)     = cards
cardsFromCombination (Pair cards _)         = cards
cardsFromCombination (ThreeOfAKind cards _) = cards
cardsFromCombination (Straight cards _)     = cards
cardsFromCombination (FullHouse cards _)    = cards
cardsFromCombination (Stairs cards _)       = cards
cardsFromCombination (Bomb cards _)         = cards

canBePlayedOnTop :: TichuCombination -> TichuCombination -> Bool
canBePlayedOnTop (SingleCard _) (SingleCard [Phoenix]) = error "Value of Phoenix is not known."
canBePlayedOnTop (SingleCard [card]) (SingleCard [cardOnBoard]) = card > cardOnBoard
canBePlayedOnTop (Pair _ val) (Pair _ valOnBoard) = val > valOnBoard
canBePlayedOnTop (ThreeOfAKind _ val) (ThreeOfAKind _ valOnBoard) = val > valOnBoard
canBePlayedOnTop (Straight cards val) (Straight cardsOnBoard valOnBoard) = val > valOnBoard && length cards == length cardsOnBoard
canBePlayedOnTop (FullHouse _ val) (FullHouse _ valOnBoard) = val > valOnBoard
canBePlayedOnTop (Stairs cards val) (Stairs cardsOnBoard valOnBoard) = val > valOnBoard && length cards == length cardsOnBoard
canBePlayedOnTop (Bomb cards val) (Bomb cardsOnBoard valOnBoard) = val > valOnBoard && length cards >= length cardsOnBoard
canBePlayedOnTop _ _ = False

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
    | maximumValue == Ace = Ace
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
  , playerTypes  :: [GamePlayer]
  , teamNames    :: [TeamName]
  , scoreLimit   :: Score
  }
  deriving (Show, Eq)

type Distribution = Map PlayerName (Map PlayerName TichuCard)

nextInOrder :: Game -> PlayerName -> PlayerName
nextInOrder game pn =
  let pns = sittingOrder $ gameConfig game
      index = fromJust (assert (pn `elem` pns) elemIndex pn pns)
      newIndex :: Int -> Int
      newIndex i =
        let
          playersHands = Map.elems $ hands game
          indexCandidate = (i + 1) `mod` length pns
         in
          case playersHands !! indexCandidate of
            [] -> newIndex indexCandidate -- skip player if he has no cards
            _  -> indexCandidate
   in pns !! newIndex index

emptyDistribution :: [PlayerName] -> Distribution
emptyDistribution pns = Map.fromList [(pn, Map.empty) | pn <- pns]

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

data GamePlayer = CLIPlayer | RandomPlayer
  deriving (Show, Eq, Read)

class Playable a where
  pickPlayerAction :: a -> Game -> [PlayerAction] -> PlayerName -> IO PlayerAction

instance Playable GamePlayer where
  pickPlayerAction CLIPlayer    = pickPlayerActionCLI
  pickPlayerAction RandomPlayer = pickPlayerActionRandom

pickPlayerActionRandom :: Game -> [PlayerAction] -> PlayerName -> IO PlayerAction
pickPlayerActionRandom _ allPossibleActions _ = do
  let possibleActions = filter (Stop /=) allPossibleActions
  if null possibleActions
    then error "No possible actions."
    else do
      index <- randomRIO (0, length possibleActions - 1)
      return $ possibleActions !! index

pickPlayerActionCLI :: Game -> [PlayerAction] -> PlayerName -> IO PlayerAction
pickPlayerActionCLI game allPossibleActions pn = do
  showPlayerInfo
  putStrLnQI "Combination to beat:"
  putStrLnQI $ showLastPlayedCards game
  putStrLnQI "Full board:"
  printQI $ map cardsFromCombination (board game)
  putStrLnQI ("Hands of player " ++ show pn ++ ":")
  printQI (hands game Map.! pn)
  askForPlayerAction pn allPossibleActions
 where
  showPlayerInfo :: IO ()
  showPlayerInfo =
    let currentPlayer = getCurrentPlayer game
     in if pn == currentPlayer
          then
            putStrLnQ
              ( "It is players "
                  ++ show currentPlayer
                  ++ " turn. What do you want to do?"
              )
          else
            putStrLnQ
              ( "Does player "
                  ++ show pn
                  ++ " want to do something before player "
                  ++ show currentPlayer
                  ++ " does its turn?"
              )

data TichuType = Tichu | GrandTichu
  deriving (Show, Eq)

isTichu :: Maybe TichuType -> Bool
isTichu (Just Tichu) = True
isTichu _            = False

isGrandTichu :: Maybe TichuType -> Bool
isGrandTichu (Just GrandTichu) = True
isGrandTichu _                 = False

data PlayerAction
  = Pass
  | Play TichuCombination
  | CallTichu
  | CallGrandTichu
  | Stop
  deriving (Show, Eq, Read)

data Game = Game
  { gameConfig     :: GameConfig
  , hands          :: Map PlayerName TichuCards
  , tricks         :: Map PlayerName TichuCards
  , board          :: [TichuCombination]
  , gamePhase      :: GamePhase
  , tichus         :: Map PlayerName (Maybe TichuType)
  , scores         :: Map TeamName Score
  , currentDealer  :: PlayerName
  , finishOrder    :: [PlayerName]
  , shouldGameStop :: Bool
  , gamePlayers    :: Map PlayerName GamePlayer
  }
  deriving (Show, Eq)

playersByTeam :: Game -> Map TeamName [PlayerName]
playersByTeam game =
  let playersByTeamList = case sittingOrder $ gameConfig game of
        [p1, p2, p3, p4] -> [[p1, p3], [p2, p4]]
        _ -> error "Not yet implemented for others than 4 players."
   in Map.fromList $ zip (teamNames $ gameConfig game) playersByTeamList

showLastPlayedCards :: Game -> String
showLastPlayedCards game = case board game of
  []             -> "Empty board"
  (lastComb : _) -> show $ cardsFromCombination lastComb

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

initialTichus :: [PlayerName] -> Map PlayerName (Maybe TichuType)
initialTichus names = Map.fromList [(n, Nothing) | n <- names]

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
    , finishOrder = []
    , shouldGameStop = False
    , gamePlayers = Map.fromList [(n, gp) | (n, gp) <- zip (playerNames config) (playerTypes config)]
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
      , tichus = setNothing $ tichus game
      , currentDealer = nextInOrder game (currentDealer game)
      }

currentDealerIndex :: Game -> Int
currentDealerIndex game =
  let index = elemIndex (currentDealer game) (sittingOrder $ gameConfig game)
   in assert (isJust index) fromJust index

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
     in game{hands = Map.map sort hands', gamePhase = Dealing deck'}
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
     in game'{gamePhase = Distributing, hands = Map.map sort (hands game')}
  r -> error $ "Wrong round: " ++ show r

-- From https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
  | p v = return v
  | otherwise = f v >>= iterateUntilM p f

getGameConfig :: IO GameConfig
getGameConfig = do
  players <- getPlayers
  pts <- getPlayerTypes
  teams <- getTeamNames
  GameConfig players pts teams <$> getMaxScore

defaultPlayerTypes :: [GamePlayer]
defaultPlayerTypes = [CLIPlayer, RandomPlayer, RandomPlayer, RandomPlayer]

getPlayerTypes :: IO [GamePlayer]
getPlayerTypes =
  putStrLnQ ("Enter player types separated by spaces (default: " ++ show defaultPlayerTypes ++ ").")
    -- TODO: List possible options
    >> getTrimmedLine
    >>= processInput
 where
  processInput :: String -> IO [GamePlayer]
  processInput rawInput
    | rawInput == quitSymbol = exitSuccess
    | otherwise =
        let
          playerTypesRaw = words rawInput
         in
          case playerTypesRaw of
            [] -> echoPlayerTypes defaultPlayerTypes
            [_, _, _, _] -> case mapM read playerTypesRaw of
              Nothing -> putStrLnE "Wrong kind of player type." >> getPlayerTypes -- TODO: List possible options
              Just pts -> echoPlayerTypes pts >> return pts
            _ -> putStrLnE "Wrong number of player types, should be 4." >> getPlayerTypes
  echoPlayerTypes :: [GamePlayer] -> IO [GamePlayer]
  echoPlayerTypes pn = putStrLnA ("Player types chosen: " ++ show pn) >> return pn

getPlayers :: IO [PlayerName]
getPlayers =
  putStrLnQ ("Enter player names separated by spaces (default: " ++ show defaultPlayerNames ++ "). This will also be the sitting order:")
    >> getTrimmedLine
    >>= processInput
 where
  processInput :: String -> IO [PlayerName]
  processInput rawInput
    | rawInput == quitSymbol = exitSuccess
    | otherwise =
        let
          players = words rawInput
         in
          case players of
            [] -> echoPlayers defaultPlayerNames
            [_, _, _, _] ->
              if nub players /= players
                then putStrLnE "Player names must be unique." >> getPlayers
                else echoPlayers players
            _ -> putStrLnE "Wrong number of players, should be 4." >> getPlayers
  echoPlayers :: [PlayerName] -> IO [PlayerName]
  echoPlayers pn = putStrLnA ("Player names chosen: " ++ show pn) >> return pn

getTeamNames :: IO [TeamName]
getTeamNames =
  putStrLnQ ("Enter team names separated by spaces (default: " ++ show defaultTeamNames ++ "):")
    >> getTrimmedLine
    >>= processInput
 where
  processInput :: String -> IO [TeamName]
  processInput rawInput
    | rawInput == quitSymbol = exitSuccess
    | otherwise =
        let
          teams = words rawInput
         in
          case teams of
            [] -> echoTeams defaultTeamNames
            [_, _] ->
              if nub teams /= teams
                then putStrLnE "Team names must be unique." >> getTeamNames
                else echoTeams teams
            _ -> putStrLnE "Wrong number of teams, should be 2." >> getTeamNames
  echoTeams :: [TeamName] -> IO [TeamName]
  echoTeams tn = putStrLnA ("Team names chosen: " ++ show tn) >> return tn

getMaxScore :: IO Int
getMaxScore =
  putStrLnQ ("Enter max score (default: " ++ show defaultScoreLimit ++ "):")
    >> getTrimmedLine
    >>= processInput
 where
  processInput :: String -> IO Int
  processInput rawInput
    | rawInput == "" = echoScore defaultScoreLimit
    | rawInput == quitSymbol = exitSuccess
    | otherwise =
        let userInput = readMaybe rawInput
         in case userInput of
              Just x -> echoScore x
              Nothing -> putStrLnE "Wrong input should be a positive number." >> getMaxScore
  echoScore :: Int -> IO Int
  echoScore sl = putStrLnA ("Score limit chosen: " ++ show sl) >> return sl

isValidForBoard :: [TichuCombination] -> TichuCombination -> Bool
isValidForBoard [] _ = True
isValidForBoard ((SingleCard [Phoenix]) : rest) combi = isValidForBoard rest combi
isValidForBoard (boardCombi : _) combi = canBePlayedOnTop combi boardCombi

possiblePlayerActions :: Game -> PlayerName -> [PlayerAction]
possiblePlayerActions game pn =
  let defaultActions = Stop : [CallTichu | canStillCallTichu game pn]
   in case gamePhase game of
        Playing currentPlayer _ ->
          let combinations =
                filter
                  (isValidForBoard $ board game)
                  ( possibleCombinations $
                      hands game Map.! pn
                  )
           in defaultActions
                ++ if pn == currentPlayer
                  then map Play combinations ++ pass
                  else
                    map
                      Play
                      ( filter
                          ( \case
                              Bomb _ _ -> not $ null (board game)
                              _        -> False
                          )
                          combinations
                      )
                      ++ [Pass]
        Dealing _ -> defaultActions -- TODO: Add GrandTichu
        Distributing -> defaultActions
        _ -> []
 where
  pass :: [PlayerAction]
  pass = [Pass | not $ null $ board game]

askForPlayerAction :: PlayerName -> [PlayerAction] -> IO PlayerAction
askForPlayerAction playerName possibleActions = do
  putStrLnQI "Possible actions:"
  foldM_ printWithNumber 0 possibleActions
  putStrLnQI "Enter the number of the desired action:"
  rawInput <- getTrimmedLine
  case rawInput of
    "" ->
      if Pass `elem` possibleActions
        then putStrLnA "Passing" >> return Pass
        else
          putStrLnE "You are not allowed to pass!"
            >> askForPlayerAction playerName possibleActions
    _ -> do
      maybeAction <- selectFromList possibleActions rawInput
      case maybeAction of
        Nothing -> askForPlayerAction playerName possibleActions
        Just Stop ->
          putStrLnQ
            (show playerName ++ " wanted to exit. Thank you for playing.")
            >> exitSuccess
        Just action -> putStrLnA ("Player " ++ show playerName ++ " played: " ++ show action) >> return action

selectFromList :: (Eq a) => [a] -> String -> IO (Maybe a)
selectFromList selectionList rawInput = do
  case readMaybe rawInput of
    Nothing ->
      putStrLnE "You must enter a number!"
        >> return Nothing
    Just selectionNumber ->
      if selectionNumber < 0 || selectionNumber >= length selectionList
        then
          putStrLnE
            ( "Invalid action number! Must be between 0 and "
                ++ show (length selectionList - 1)
                ++ "."
            )
            >> return Nothing
        else do
          let selection = selectionList !! selectionNumber
          if selection `elem` selectionList
            then return $ Just selection
            else
              putStrLnE "Invalid action"
                >> return Nothing

getCurrentPlayerWithPasses :: Game -> (PlayerName, Passes)
getCurrentPlayerWithPasses game = case gamePhase game of
  Playing playerName numberOfPassesBefore -> (playerName, numberOfPassesBefore)
  _ -> error "Wrong game phase should be Playing"

getActivePlayers :: Game -> [PlayerName]
getActivePlayers game = case sittingOrder (gameConfig game) \\ finishOrder game of
  [_] -> [] -- only one player left: game finished
  ps  -> ps

getCurrentPlayer :: Game -> PlayerName
getCurrentPlayer = fst . getCurrentPlayerWithPasses

playerListWithCurrentPlayerFirst :: Game -> [PlayerName]
playerListWithCurrentPlayerFirst game =
  let playerList = getActivePlayers game
      i = fromJust $ elemIndex (getCurrentPlayer game) playerList
   in drop i playerList ++ take i playerList

getGamePlayersFromNames :: Game -> [PlayerName] -> [GamePlayer]
getGamePlayersFromNames game = map (\p -> gamePlayers game Map.! p)

getPlayerActions :: Game -> IO (Map PlayerName PlayerAction)
getPlayerActions game = do
  let players = playerListWithCurrentPlayerFirst game
  actions <-
    mapM (getPlayerActionsByName game) players
  return $ Map.fromList $ zip players actions

getPlayerActionsByName :: Game -> PlayerName -> IO PlayerAction
getPlayerActionsByName game pn =
  let allPossibleActions = possiblePlayerActions game pn
   in pickPlayerAction (gamePlayers game Map.! pn) game allPossibleActions pn

canStillCallTichu :: Game -> PlayerName -> Bool
canStillCallTichu game pn = length (hands game Map.! pn) == maxCards && isNothing (tichus game Map.! pn)

update :: Game -> IO Game
update game = do
  case gamePhase game of
    Starting                     -> startGame game
    Dealing _                    -> return $ dealAllCards game
    Distributing                 -> distribute game
    Playing _ _                  -> play game
    -- Playing _ _                  -> askForTichu game >>= play
    NextRound                    -> nextRound game
    GiveAwayLooserTricksAndHands -> return $ giveAwayLooserTricksAndHands game
    Scoring                      -> return $ score game
    Finished                     -> finish game

giveAwayLooserTricksAndHands :: Game -> Game
giveAwayLooserTricksAndHands game = case playerNames' game \\ finishOrder game of
  [looser] ->
    let winner = head $ finishOrder game
        newTricks = Map.insert looser [] (tricks game)
        newTricks' =
          Map.insert
            winner
            ( (tricks game Map.! winner)
                ++ (tricks game Map.! looser)
            )
            newTricks
        playerOtherTeam = head $ head $ filter (notElem looser) (Map.elems $ playersByTeam game)
        newTricks'' = Map.insert playerOtherTeam ((tricks game Map.! playerOtherTeam) ++ (hands game Map.! looser)) newTricks'
        newHands = Map.insert looser [] (hands game)
     in game
          { tricks = newTricks''
          , hands = newHands
          , gamePhase = Scoring
          }
  _ -> error ("There should be 3 player in finishing order found: " ++ show (finishOrder game))

applyPlayerAction :: Game -> PlayerName -> PlayerAction -> Game
applyPlayerAction game pn playerAction =
  -- TODO: Finish implementation
  let (currentPlayer, passes) = getCurrentPlayerWithPasses game
   in case playerAction of
        Play combination ->
          let playerHand = hands game Map.! currentPlayer
              cards = cardsFromCombination combination
              newPlayerHand = playerHand \\ cards
              newHands = Map.insert currentPlayer newPlayerHand $ hands game
              newFinishOrder = if null newPlayerHand then currentPlayer : finishOrder game else finishOrder game
              endOfRound = case length newFinishOrder of
                3 -> True
                2 -> any (all (`elem` newFinishOrder)) (Map.elems $ playersByTeam game) -- match
                _ -> False
              newGamePhase =
                if endOfRound
                  then Scoring
                  else Playing (nextInOrder game currentPlayer) 0
           in game
                { hands = newHands
                , board =
                    combination
                      : board game
                , gamePhase = newGamePhase
                , finishOrder = newFinishOrder
                }
        Pass ->
          if pn == currentPlayer
            then
              if passes < 2
                then game{gamePhase = Playing (nextInOrder game currentPlayer) (passes + 1)}
                else -- FIXME: Give away trick with dragon!

                  let nextPlayer = nextInOrder game currentPlayer
                      trickNextPlayer = tricks game Map.! nextPlayer
                      newTrick = concatMap cardsFromCombination (board game) ++ trickNextPlayer
                      newBoard = []
                      newPasses = 0
                      newGamePhase = Playing nextPlayer newPasses
                   in game
                        { gamePhase = newGamePhase
                        , board = newBoard
                        , tricks = Map.insert nextPlayer newTrick $ tricks game
                        }
            else game
        Stop -> game{shouldGameStop = True}
        CallTichu -> game{tichus = Map.insert pn (Just Tichu) (tichus game)}
        CallGrandTichu -> game{tichus = Map.insert pn (Just GrandTichu) (tichus game)}

play :: Game -> IO Game
play game =
  foldM (\g pn -> getPlayerActionsByName g pn >>= updateGameByPlayerAction g pn) game (sortBy currentPlayerFirst (getActivePlayers game))
 where
  currentPlayerFirst :: PlayerName -> PlayerName -> Ordering
  currentPlayerFirst pn' pn'' = case gamePhase game of
    Playing currentPlayer _ -> if pn' == currentPlayer then LT else if pn'' == currentPlayer then GT else EQ
    _ -> EQ

updateGameByPlayerAction :: Game -> PlayerName -> PlayerAction -> IO Game
updateGameByPlayerAction game pn pa =
  let game' = applyPlayerAction game pn pa
   in if pa `elem` [CallTichu, CallGrandTichu]
        then do
          putStrLnA ("Player " ++ show pn ++ " called " ++ if pa == CallTichu then "Tichu" else "Grand Tichu")
          pa' <- getPlayerActionsByName game' pn
          return $ applyPlayerAction game' pn pa'
        else return game'

distribute :: Game -> IO Game
distribute game = return game{gamePhase = Playing (startingPlayer $ hands game) 0} -- TODO: Implement

startingPlayer :: Map PlayerName TichuCards -> PlayerName
startingPlayer = head . Map.keys . Map.filter (elem Mahjong)

display :: Game -> IO ()
display game =
  putStrLnQ
    "Game State:"
    >> printQI
      (gamePhase game)
    >> putStrLnQI ("Board: " ++ show (board game))
    >> putStrLnQI ("Hands: " ++ show (Map.toList $ hands game))
    >> putStrLnQI ("Tricks: " ++ show (Map.toList $ tricks game))
    >> putStrLnQI ("Score: " ++ show (Map.toList $ scores game))

matchBonus :: Int
matchBonus = 200

tichuBonus :: Int
tichuBonus = 100

grandTichuBonus :: Int
grandTichuBonus = 200

score :: Game -> Game
score game =
  let
    newScore =
      Map.mapWithKey
        scorePerTeam
        (playersByTeam game)
    newGamePhase = if any (>= scoreLimit (gameConfig game)) (Map.elems newScore) then Finished else NextRound
   in
    game{scores = newScore, gamePhase = newGamePhase}
 where
  scorePerTeam :: TeamName -> [PlayerName] -> Int
  scorePerTeam teamName playersInTeam =
    let currentScore = scores game
        currentTricks = tricks game
        currentScoreForTeam = currentScore Map.! teamName
        scoreFromCardsOrMatch =
          if all (`elem` finishOrder game) playersInTeam
            then matchBonus
            else
              sum
                ( map
                    ( \pn -> cardsScore (currentTricks Map.! pn)
                    )
                    playersInTeam
                )
        winner = head $ finishOrder game
        scoreForTichu =
          if winner `elem` playersInTeam
            then
              if isTichu (tichus game Map.! winner)
                then tichuBonus
                else
                  if isGrandTichu (tichus game Map.! winner)
                    then grandTichuBonus
                    else 0
            else 0
        scoreForFailedTichu =
          sum $
            Map.mapWithKey
              ( \pn t ->
                  if pn == winner || notElem pn playersInTeam
                    then 0
                    else
                      if isTichu t
                        then -tichuBonus
                        else if isGrandTichu t then -grandTichuBonus else 0
              )
              (tichus game)
     in currentScoreForTeam + scoreFromCardsOrMatch + scoreForTichu + scoreForFailedTichu

finish :: Game -> IO Game
finish game = do
  let winningTeams = Map.toList $ Map.filter (>= scoreLimit (gameConfig game)) (scores game)
  printWinners winningTeams
  return game
 where
  printWinners :: [(TeamName, Score)] -> IO ()
  printWinners [] = error "No team won!"
  printWinners [(winner, winningScore)] = putStrLnQ ("Yeay we have a winner! Team " ++ show winner ++ " won with a score of " ++ show winningScore ++ " points.")
  printWinners winners = putStrLnQ ("Yeeeey, we have more than one winner! The teams " ++ show winners)

run :: Game -> IO Game
run game = display game >> update game

playTichu :: IO ()
playTichu = getGameConfig >>= iterateUntilM shouldGameStop run . newGame >>= display
