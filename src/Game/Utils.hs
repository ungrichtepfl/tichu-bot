module Game.Utils (module Game.Utils) where

import Control.Exception (assert)
import Data.List (elemIndex, (\\))
import Data.Map (Map)
import Data.Maybe (fromJust)
import System.Exit (exitSuccess)

import qualified Data.Map as Map

import Game.Structures

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

nonPhoenixCards :: TichuCards -> TichuCards
nonPhoenixCards = filter (/= Phoenix)

containsSpecialCards :: TichuCards -> Bool
containsSpecialCards cards = any (`elem` cards) [Dragon, Phoenix, Mahjong, Dog]

containsSpecialCardsNoPhoenix :: TichuCards -> Bool
containsSpecialCardsNoPhoenix cards = any (`elem` cards) [Dragon, Mahjong, Dog]

noNothings :: (Eq a) => [Maybe a] -> Bool
noNothings = all (`notElem` Nothing)

nextInOrder :: Game -> PlayerName -> Bool -> PlayerName
nextInOrder game pn dogPlayed =
    let pns = sittingOrder $ gameConfig game
        index = fromJust (assert (pn `elem` pns) elemIndex pn pns)
        index' =
            if dogPlayed
                then
                    (index + (length pns `div` 2 - 1)) -- Skip player if dog is played
                        `mod` length pns
                else index
        newIndex :: Int -> Int
        newIndex i =
            let
                indexCandidate = (i + 1) `mod` length pns
             in
                if pns !! indexCandidate `elem` finishOrder game
                    then
                        newIndex indexCandidate -- skip player already finished
                    else indexCandidate
     in pns !! newIndex index'

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

getCurrentPlayerWithPassesAndPlayerToBeat :: Game -> (PlayerName, Passes, PlayerToBeat)
getCurrentPlayerWithPassesAndPlayerToBeat game = case gamePhase game of
    Playing playerName numberOfPassesBefore playerToBeat -> (playerName, numberOfPassesBefore, playerToBeat)
    _ -> error "Wrong game phase should be Playing"

getActivePlayers :: Game -> [PlayerName]
getActivePlayers game = case sittingOrder (gameConfig game) \\ finishOrder game of
    [_] -> [] -- only one player left: game finished
    ps -> ps

getCurrentPlayer :: Game -> PlayerName
getCurrentPlayer g =
    let (n, _, _) = getCurrentPlayerWithPassesAndPlayerToBeat g
     in n

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
