module Game.Utils (module Game.Utils) where

import Control.Exception (assert)
import Control.Monad (forM)
import Data.Array.IO (
    IOArray,
    newListArray,
    readArray,
    writeArray,
 )
import Data.List (elemIndex, (\\))
import Data.Map (Map)
import Data.Maybe (fromJust)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

import qualified Data.Map as Map

import Game.Structures

-- From https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html
iterateUntilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntilM p f v
    | p v = return v
    | otherwise = f v >>= iterateUntilM p f

iterateUntilM_ :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateUntilM_ p f v
    | p v = return ()
    | otherwise = f v >>= iterateUntilM_ p f

exitGame :: IO a
exitGame = putStrLn "Exiting Game. Thank you for playing." >> exitSuccess

setEmpty :: Map k [a] -> Map k [a]
setEmpty = Map.map (const [])

setFalse :: Map k Bool -> Map k Bool
setFalse = Map.map (const False)

setNothing :: Map k (Maybe a) -> Map k (Maybe a)
setNothing = Map.map (const Nothing)

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

nonPhoenixCards :: TichuCards -> TichuCards
nonPhoenixCards = filter (/= Phoenix)

containsSpecialCards :: TichuCards -> Bool
containsSpecialCards cards = any (`elem` cards) [Dragon, Phoenix, Mahjong, Dog]

containsSpecialCardsNoPhoenix :: TichuCards -> Bool
containsSpecialCardsNoPhoenix cards = any (`elem` cards) [Dragon, Mahjong, Dog]

noNothings :: (Eq a) => [Maybe a] -> Bool
noNothings = all (`notElem` Nothing)

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
                    _ -> indexCandidate
     in pns !! newIndex index

emptyDistribution :: [PlayerName] -> Distribution
emptyDistribution pns = Map.fromList [(pn, Map.empty) | pn <- pns]

playersByTeam :: Game -> Map TeamName [PlayerName]
playersByTeam game =
    let playersByTeamList = case sittingOrder $ gameConfig game of
            [p1, p2, p3, p4] -> [[p1, p3], [p2, p4]]
            _ -> error "Not yet implemented for others than 4 players."
     in Map.fromList $ zip (teamNames $ gameConfig game) playersByTeamList

isTichu :: Maybe TichuType -> Bool
isTichu (Just Tichu) = True
isTichu _ = False

isGrandTichu :: Maybe TichuType -> Bool
isGrandTichu (Just GrandTichu) = True
isGrandTichu _ = False

cardsScore :: [TichuCard] -> Int
cardsScore = sum . map cardValue

cardsFromCombination :: TichuCombination -> TichuCards
cardsFromCombination (SingleCard cards) = cards
cardsFromCombination (Pair cards _) = cards
cardsFromCombination (ThreeOfAKind cards _) = cards
cardsFromCombination (Straight cards _) = cards
cardsFromCombination (FullHouse cards _) = cards
cardsFromCombination (Stairs cards _) = cards
cardsFromCombination (Bomb cards _) = cards

playerNames :: GameConfig -> [PlayerName]
playerNames = sittingOrder

playerNames' :: Game -> [PlayerName]
playerNames' = sittingOrder . gameConfig

getCurrentPlayerWithPasses :: Game -> (PlayerName, Passes)
getCurrentPlayerWithPasses game = case gamePhase game of
    Playing playerName numberOfPassesBefore -> (playerName, numberOfPassesBefore)
    _ -> error "Wrong game phase should be Playing"

getActivePlayers :: Game -> [PlayerName]
getActivePlayers game = case sittingOrder (gameConfig game) \\ finishOrder game of
    [_] -> [] -- only one player left: game finished
    ps -> ps

getCurrentPlayer :: Game -> PlayerName
getCurrentPlayer = fst . getCurrentPlayerWithPasses

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
