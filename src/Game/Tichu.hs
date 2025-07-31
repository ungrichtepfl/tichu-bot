{-# LANGUAGE LambdaCase #-}

module Game.Tichu (module Game.Tichu) where

import Control.Exception (assert)
import Data.List (elemIndex, sort, (\\))
import Data.Map (Map)
import Data.Maybe (fromJust, isJust, isNothing)
import System.Random (mkStdGen)

import qualified Data.Map as Map

import Game.Combinations
import Game.Constants
import Game.Structures
import Game.Utils

orderedDeck :: TichuCards
orderedDeck =
    [ PokerCard (v, c) | v <- [Two .. Ace], c <- [Red .. Black]
    ]
        ++ [Dragon, Phoenix, Mahjong, Dog]

initialHands :: [PlayerName] -> Map PlayerName TichuCards
initialHands names = Map.fromList [(n, []) | n <- names]

initialTricks :: [PlayerName] -> Map PlayerName TichuCards
initialTricks names = Map.fromList [(n, []) | n <- names]

initialTichus :: [PlayerName] -> Map PlayerName (Maybe TichuType)
initialTichus names = Map.fromList [(n, Nothing) | n <- names]

initialScores :: [TeamName] -> Map TeamName Score
initialScores names = Map.fromList [(n, 0) | n <- names]

newGame :: GameConfig -> Int -> (Game, Maybe (Map PlayerName [PlayerAction]))
newGame config seed =
    let gen = mkStdGen seed
        game =
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
                , winnerTeams = []
                , generator = gen
                }
     in (game, allPossiblePlayerActions game)

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

isValidForBoard :: [TichuCombination] -> TichuCombination -> Bool
isValidForBoard [] _ = True
isValidForBoard ((SingleCard [Phoenix]) : rest) combi = isValidForBoard rest combi
isValidForBoard (boardCombi : _) combi = canBePlayedOnTop combi boardCombi

allPossiblePlayerActions :: Game -> Maybe (Map PlayerName [PlayerAction])
allPossiblePlayerActions game =
    let pln = playerNames' game
        actions = possiblePlayerActions game <$> pln
     in if all null actions
            then Nothing
            else Just $ Map.fromList $ zip pln actions

possiblePlayerActions :: Game -> PlayerName -> [PlayerAction]
possiblePlayerActions game pn =
    let defaultActions = Stop : [CallTichu | canStillCallTichu game pn]
     in case gamePhase game of
            Playing currentPlayer _ ->
                let combinations =
                        filter
                            (isValidForBoard $ board game)
                            (possibleCombinations $ hands game Map.! pn)
                 in defaultActions
                        ++ if pn == currentPlayer
                            then map Play combinations ++ pass
                            else
                                map
                                    Play
                                    ( filter
                                        ( \case
                                            Bomb _ _ -> not $ null (board game)
                                            _ -> False
                                        )
                                        combinations
                                    )
                                    ++ [Pass]
            Dealing _ -> [] -- TODO: Add GrandTichu
            _ -> []
  where
    pass :: [PlayerAction]
    pass = [Pass | not $ null $ board game]

playerListWithCurrentPlayerFirst :: Game -> [PlayerName]
playerListWithCurrentPlayerFirst game =
    let playerList = getActivePlayers game
        i = fromJust $ elemIndex (getCurrentPlayer game) playerList
     in drop i playerList ++ take i playerList

canStillCallTichu :: Game -> PlayerName -> Bool
canStillCallTichu game pn = length (hands game Map.! pn) == maxCards && isNothing (tichus game Map.! pn)

applyPlayerAction :: Game -> (PlayerName, PlayerAction) -> Game
applyPlayerAction game (pn, playerAction) =
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

startGame :: Game -> Game
startGame game =
    let
        (initialDeck, gen') = shuffle orderedDeck (generator game)
        (shuffledPlayers, gen'') = shuffle (playerNames' game) gen'
        randomPlayer = head shuffledPlayers
     in
        game
            { gamePhase = Dealing initialDeck
            , currentDealer = randomPlayer
            , generator = gen''
            }

distribute :: Game -> (PlayerName, PlayerAction) -> Game
distribute game _ = game{gamePhase = Playing (startingPlayer $ hands game) 0} -- TODO: Implement

nextRound :: Game -> Game
nextRound game = do
    let
        (initialDeck, gen') = shuffle orderedDeck (generator game)
     in
        game
            { hands = setEmpty $ hands game
            , tricks = setEmpty $ tricks game
            , gamePhase = Dealing initialDeck
            , tichus = setNothing $ tichus game
            , currentDealer = nextInOrder game (currentDealer game)
            , generator = gen'
            }

finish :: Game -> Game
finish game = do
    game{shouldGameStop = True}

updateGame :: Game -> Maybe (PlayerName, PlayerAction) -> (Game, Maybe (Map PlayerName [PlayerAction]))
updateGame game playersAction =
    let
        game' = case gamePhase game of
            Starting -> startGame game
            Dealing _ -> dealAllCards game
            Distributing ->
                ( case playersAction of
                    Just action ->
                        distribute game action
                    -- Nothing -> game -- Nothing todo
                    Nothing -> -- TODO: Remove and uncomment above as soon as distribute is implemented
                        game{gamePhase = Playing (startingPlayer $ hands game) 0} -- TODO: Implement
                )
            Playing _ _ ->
                ( case playersAction of
                    Just action ->
                        applyPlayerAction game action
                    Nothing -> game -- Nothing todo
                )
            NextRound -> nextRound game
            GiveAwayLooserTricksAndHands -> giveAwayLooserTricksAndHands game
            Scoring -> score game
            Finished -> finish game
     in
        (game', allPossiblePlayerActions game')

startingPlayer :: Map PlayerName TichuCards -> PlayerName
startingPlayer = head . Map.keys . Map.filter (elem Mahjong)

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

score :: Game -> Game
score game =
    let newScore =
            Map.mapWithKey
                scorePerTeam
                (playersByTeam game)
        newGamePhase = if any (>= scoreLimit (gameConfig game)) (Map.elems newScore) then Finished else NextRound
        winnerTeams' = fst <$> Map.toList (Map.filter (>= scoreLimit (gameConfig game)) (scores game))
     in game{scores = newScore, gamePhase = newGamePhase, winnerTeams = winnerTeams'}
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
                                (\pn -> cardsScore (currentTricks Map.! pn))
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
