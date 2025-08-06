module Game.Combinations (module Game.Combinations) where

import Data.List (nub, nubBy, sort, tails, (\\))
import Data.Maybe (mapMaybe)

import Game.Structures
import Game.Utils

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
            let numCards = length cards'
                values = mapMaybe value cards'
                minVal = minimum values
                maxVal = maximum values
                values' = [minVal .. maxVal]
                values'' = values' \\ values
             in if Phoenix `elem` cards'
                    then length values'' <= 1 && nub cards' == cards' && nub values == values
                    else numCards == length values' && nub values == values

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
