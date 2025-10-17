{-# LANGUAGE LambdaCase #-}

module Game.Tichu (module Game.Tichu) where

import Control.Exception (assert)
import Control.Monad.Loops (iterateUntilM)
import Data.List (elemIndex, sort, (\\))
import Data.Map (Map)
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import System.Random (StdGen, mkStdGen, uniformShuffleList)

import qualified Data.Map as Map

import Bots.Random
import Game.Combinations
import Game.Constants
import Game.Interface
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

initialGame :: GameConfig -> Int -> (Game, Maybe (Map PlayerName [PlayerAction]))
initialGame config seed =
    let gen = mkStdGen seed
        game = newGame gen config
     in (game, allPossiblePlayerActions game)

newGame :: StdGen -> GameConfig -> Game
newGame gen config =
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
            Playing currentPlayer _ _ ->
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

applyPlayerAction :: Game -> (PlayerName, PlayerAction) -> PlayerName -> Passes -> PlayerToBeat -> Game
applyPlayerAction game (pn, playerAction) currentPlayer passes mPlayerToBeat =
    -- TODO: Finish implementation
    case playerAction of
        Play combination ->
            let playerHand = hands game Map.! currentPlayer
                cards = cardsFromCombination combination
                newPlayerHand = playerHand \\ cards
                newHands = Map.insert currentPlayer newPlayerHand $ hands game
                newFinishOrder = if null newPlayerHand then currentPlayer : finishOrder game else finishOrder game
                numPlayers = length (sittingOrder $ gameConfig game)
                endOfRound =
                    if length newFinishOrder
                        == numPlayers - 1
                        then True
                        else
                            if length newFinishOrder == numPlayers - 2
                                then any (all (`elem` newFinishOrder)) (Map.elems $ playersByTeam game) -- match
                                else False
                isDog = combination == SingleCard [Dog]
                newGamePhase =
                    if endOfRound
                        then GiveAwayLooserTricksAndHands
                        else Playing (nextInOrder game currentPlayer isDog) 0 (if isDog then Nothing else Just currentPlayer)
             in game
                    { hands = newHands
                    , board =
                        if not isDog
                            then
                                combination
                                    : board game
                            else []
                    , tricks =
                        if not isDog
                            then tricks game
                            else
                                Map.insert
                                    pn
                                    (cardsFromCombination combination ++ (tricks game) Map.! pn)
                                    (tricks game)
                    , gamePhase = newGamePhase
                    , finishOrder = newFinishOrder
                    }
        Pass ->
            case mPlayerToBeat of
                Nothing -> error "There must be a player to beat"
                Just playerToBeat ->
                    if pn == currentPlayer
                        then
                            let playerToBeatNoCards = null (hands game Map.! playerToBeat)
                                numPasses = length (sittingOrder $ gameConfig game) - length (finishOrder game) - 2 + if playerToBeatNoCards then 1 else 0
                             in if passes < numPasses
                                    then game{gamePhase = Playing (nextInOrder game currentPlayer False) (passes + 1) mPlayerToBeat}
                                    else -- FIXME: Give away trick with dragon!
                                    -- Player to beat won
                                        let nextPlayer = nextInOrder game currentPlayer False
                                            trickPlayerToBeat = tricks game Map.! playerToBeat
                                            newTricksPlayerToBeat = concatMap cardsFromCombination (board game) ++ trickPlayerToBeat
                                            newBoard = []
                                            newPasses = 0
                                            newGamePhase = Playing nextPlayer newPasses Nothing
                                         in game
                                                { gamePhase = newGamePhase
                                                , board = newBoard
                                                , tricks = Map.insert playerToBeat newTricksPlayerToBeat $ tricks game
                                                }
                        else game
        Stop -> game{shouldGameStop = True}
        CallTichu -> game{tichus = Map.insert pn (Just Tichu) (tichus game)}
        CallGrandTichu -> game{tichus = Map.insert pn (Just GrandTichu) (tichus game)}

startGame :: Game -> Game
startGame game =
    let
        (initialDeck, gen') = uniformShuffleList orderedDeck (generator game)
        (shuffledPlayers, gen'') = uniformShuffleList (playerNames' game) gen'
        randomPlayerName = head shuffledPlayers
     in
        game
            { gamePhase = Dealing initialDeck
            , currentDealer = randomPlayerName
            , generator = gen''
            }

distribute :: Game -> (PlayerName, PlayerAction) -> Game
distribute game _ = game{gamePhase = Playing (startingPlayer $ hands game) 0 Nothing} -- TODO: Implement

nextRound :: Game -> Game
nextRound game =
    let
        config = gameConfig game
        game' = newGame (generator game) config
     in
        game'
            { scores = scores game
            , currentDealer = nextInOrder game' (currentDealer game) False
            }

finish :: Game -> Game
finish game = game -- NOTE: Just stay in the finish state until the user changes the game

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
                    Nothing ->
                        -- NOTE: Just start game if distributing is not implemented yet
                        game{gamePhase = Playing (startingPlayer $ hands game) 0 Nothing}
                )
            Playing currentPlayer passes playerToBeat ->
                ( case playersAction of
                    Just action ->
                        applyPlayerAction game action currentPlayer passes playerToBeat
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
        let winner = head (reverse $ finishOrder game)
            newTricksWinner = (tricks game Map.! winner) ++ (tricks game Map.! looser)
            newTricksWinnerLoser = Map.insert looser [] . Map.insert winner newTricksWinner $ (tricks game)
            playerOtherTeam = head $ head $ filter (notElem looser) (Map.elems $ playersByTeam game)
            handLoser = hands game Map.! looser
            newTricksOtherPlayer = (newTricksWinnerLoser Map.! playerOtherTeam) ++ handLoser
            newTricks = Map.insert playerOtherTeam newTricksOtherPlayer newTricksWinnerLoser
            newHands = Map.insert looser [] (hands game)
         in game
                { tricks = newTricks
                , hands = newHands
                , gamePhase = Scoring
                }
    [_, _] -> game{gamePhase = Scoring} -- MATCH does not matter
    _ -> error ("There should be numPlayers - 1 player in finishing order found: " ++ show (finishOrder game))

score :: Game -> Game
score game =
    let
        newGamePhase = if any (>= scoreLimit (gameConfig game)) (Map.elems newScore) then Finished else NextRound
        winnerTeams' = Map.keys $ Map.filter (>= scoreLimit (gameConfig game)) newScore
     in
        game{scores = newScore, gamePhase = newGamePhase, winnerTeams = winnerTeams'}
  where
    matchBonusForTeam :: TeamName -> Int
    matchBonusForTeam tn =
        let playersInTeam = playersByTeam game Map.! tn
         in case reverse $ finishOrder game of
                [] -> 0
                [_] -> 0
                p1 : (p2 : _) ->
                    if p1 `elem` playersInTeam && p2 `elem` playersInTeam
                        then matchBonus
                        else 0
    teams = teamNames $ gameConfig game
    matchBonusPerTeam = map matchBonusForTeam teams
    anyMatch = any (> 0) matchBonusPerTeam
    winnerPlayer = head (reverse $ finishOrder game)
    tichuBonusForTeam :: TeamName -> Int
    tichuBonusForTeam tn =
        let playersInTeam = playersByTeam game Map.! tn
         in sum (map tichuBonusForPlayer playersInTeam)
      where
        tichuBonusForPlayer pn = case tichus game Map.! pn of
            Nothing -> 0
            Just Tichu -> if pn == winnerPlayer then tichuBonus else -tichuBonus
            Just GrandTichu -> if pn == winnerPlayer then grandTichuBonus else -grandTichuBonus
    tichuBonusPerTeam = map tichuBonusForTeam teams
    trickScoreForTeam :: TeamName -> Int
    trickScoreForTeam tn
        | anyMatch = 0
        | otherwise =
            let playersInTeam = playersByTeam game Map.! tn
                teamTricks =
                    mapMaybe
                        (\pn -> if pn `elem` playersInTeam then Just (tricks game Map.! pn) else Nothing)
                        (playerNames' game)
             in sum $ map cardsScore teamTricks

    tricksScorePerTeam = map trickScoreForTeam teams
    scorePerTeam =
        zipWith3
            (\a b c -> a + b + c)
            matchBonusPerTeam
            tichuBonusPerTeam
            tricksScorePerTeam
    scoreThisRound = Map.fromList $ zip teams scorePerTeam
    newScore = Map.mapWithKey (\k v -> v + scoreThisRound Map.! k) (scores game)

configLoopStop :: (Maybe GameConfig, Bool) -> Bool
configLoopStop configOutput = isJust (fst configOutput) || snd configOutput

configLoopBody :: (Interface interface) => interface -> (Maybe GameConfig, Bool) -> IO (Maybe GameConfig, Bool)
configLoopBody interface _ = do
    conf <- updateDrawConfig interface
    end <- gameShouldStop interface
    return (conf, end)

configLoop :: (Interface interface) => interface -> IO (Maybe GameConfig)
configLoop interface =
    do
        (config, _) <- iterateUntilM configLoopStop (configLoopBody interface) (Nothing, False)
        return config

userPlayerIndex :: Int
userPlayerIndex = 2 -- The third player is the user

getUserPlayerName :: Game -> PlayerName
getUserPlayerName game = playerNames' game !! userPlayerIndex

getCurrentAction ::
    (Interface interface) =>
    interface -> Game -> Maybe (Map PlayerName [PlayerAction]) -> IO (Maybe (PlayerName, PlayerAction), Game)
getCurrentAction _ game Nothing = return (Nothing, game)
getCurrentAction interface game (Just playerActions) =
    case gamePhase game of
        Playing player _ _ ->
            if player == getUserPlayerName game
                then do
                    userAction <- getUserAction interface
                    return ((\a -> (player, a)) <$> userAction, game)
                else
                    let actions = playerActions Map.! player
                        (botAction, g) = randomPlayer game actions player
                     in return (Just (player, botAction), g)
        _ -> return (Nothing, game)

gameLoopBody ::
    (Interface interface) =>
    interface ->
    (Game, Maybe (Map PlayerName [PlayerAction])) ->
    IO (Game, Maybe (Map PlayerName [PlayerAction]))
gameLoopBody interface toSend@(game, possibleActions) =
    let
        restartStop (g, _) = shouldGameStop g || gamePhase g == Starting
        restartLoopBody ::
            (Game, Maybe (Map PlayerName [PlayerAction])) ->
            IO (Game, Maybe (Map PlayerName [PlayerAction]))
        restartLoopBody (g, _) = do
            let toSend' = (g, Nothing)
            updateStateAndRenderGame interface toSend'
            end <- gameShouldStop interface
            restart <- shouldGameRestart interface
            if restart && not end
                then return (newGame (generator g) (gameConfig g), Nothing)
                else return (g{shouldGameStop = end}, Nothing)
     in
        do
            let currentPlayingPlayer = case gamePhase game of
                    Playing p _ _ -> p
                    _ -> ""
            updateStateAndRenderGame interface toSend
            (action, game') <- getCurrentAction interface game possibleActions
            case action of
                Just a -> putStrLn $ ">>> " ++ fst a ++ " played : " ++ show (snd a)
                _ -> return ()
            let (game'', possibleActions') =
                    if isNothing action && currentPlayingPlayer == getUserPlayerName game'
                        then (game', possibleActions)
                        else
                            updateGame game' action
            end <- gameShouldStop interface
            let game''' = game''{shouldGameStop = end}
            case gamePhase game''' of
                NextRound -> newRound interface >> return (game''', possibleActions')
                Finished -> iterateUntilM restartStop restartLoopBody (game''', Nothing)
                _ -> return (game''', possibleActions')

gameLoopStop :: (Game, Maybe (Map PlayerName [PlayerAction])) -> Bool
gameLoopStop (game, _) = shouldGameStop game

gameLoop :: (Interface interface) => interface -> Int -> GameConfig -> IO ()
gameLoop interface seed config =
    iterateUntilM gameLoopStop (gameLoopBody interface) (initialGame config seed) >> return ()
