module Cli (module Cli) where

import Control.Monad (foldM)
import Data.List (nub, sortBy)
import Data.Map (Map)
import Text.Read (readMaybe)

import qualified Data.Map as Map

import CommandLinePlayer
import Game.Constants
import Game.Structures
import Game.Tichu
import Game.Utils
import IO

playTichu :: IO ()
playTichu = do
    gameConf <- getGameConfig
    gamePlayers <- getGamePlayers $ sittingOrder gameConf
    iterateUntilM_ shouldGameStop (update gamePlayers) (newGame gameConf)

shuffledDeck :: IO TichuCards
shuffledDeck = shuffle orderedDeck

getGamePlayers :: [PlayerName] -> IO (Map PlayerName GamePlayer)
getGamePlayers pns =
    putStrLnQ
        ( "Enter player types separated by spaces for players "
            ++ showList' pns
            ++ " (default: "
            ++ showList' (map fst defaultPlayerTypes)
            ++ ")."
        )
        -- TODO: List possible options
        >> getTrimmedLine
        >>= processInput
  where
    processInput :: String -> IO (Map PlayerName GamePlayer)
    processInput rawInput
        | rawInput == quitSymbol = exitGame
        | otherwise =
            let playerTypesRaw = words rawInput
             in case playerTypesRaw of
                    [] -> echoPlayerTypes defaultPlayerTypes
                    [_, _, _, _] -> case mapM textToPlayer playerTypesRaw of
                        Nothing -> putStrLnE "Wrong kind of player type." >> getGamePlayers pns -- TODO: List possible options
                        Just pts -> echoPlayerTypes (zip playerTypesRaw pts)
                    _ -> putStrLnE "Wrong number of player types, should be 4." >> getGamePlayers pns

    echoPlayerTypes :: [(String, GamePlayer)] -> IO (Map PlayerName GamePlayer)
    echoPlayerTypes gps = putStrLnA ("Player types chosen: " ++ showList' (map fst gps)) >> return (Map.fromList $ zip pns (map snd gps))

getPlayers :: IO [PlayerName]
getPlayers =
    putStrLnQ
        ( "Enter player names separated by spaces (default: "
            ++ showList' defaultPlayerNames
            ++ "). This will also be the sitting order:"
        )
        >> getTrimmedLine
        >>= processInput
  where
    processInput :: String -> IO [PlayerName]
    processInput rawInput
        | rawInput == quitSymbol = exitGame
        | otherwise =
            let players = words rawInput
             in case players of
                    [] -> echoPlayers defaultPlayerNames
                    [_, _, _, _] ->
                        if nub players /= players
                            then putStrLnE "Player names must be unique." >> getPlayers
                            else echoPlayers players
                    _ -> putStrLnE "Wrong number of players, should be 4." >> getPlayers
    echoPlayers :: [PlayerName] -> IO [PlayerName]
    echoPlayers pn = putStrLnA ("Player names chosen: " ++ showList' pn) >> return pn

getTeamNames :: IO [TeamName]
getTeamNames =
    putStrLnQ ("Enter team names separated by spaces (default: " ++ showList' defaultTeamNames ++ "):")
        >> getTrimmedLine
        >>= processInput
  where
    processInput :: String -> IO [TeamName]
    processInput rawInput
        | rawInput == quitSymbol = exitGame
        | otherwise =
            let teams = words rawInput
             in case teams of
                    [] -> echoTeams defaultTeamNames
                    [_, _] ->
                        if nub teams /= teams
                            then putStrLnE "Team names must be unique." >> getTeamNames
                            else echoTeams teams
                    _ -> putStrLnE "Wrong number of teams, should be 2." >> getTeamNames
    echoTeams :: [TeamName] -> IO [TeamName]
    echoTeams tn = putStrLnA ("Team names chosen: " ++ showList' tn) >> return tn

getMaxScore :: IO Int
getMaxScore =
    putStrLnQ ("Enter max score (default: " ++ show defaultScoreLimit ++ "):")
        >> getTrimmedLine
        >>= processInput
  where
    processInput :: String -> IO Int
    processInput rawInput
        | rawInput == "" = echoScore defaultScoreLimit
        | rawInput == quitSymbol = exitGame
        | otherwise =
            let userInput = readMaybe rawInput
             in case userInput of
                    Just x -> echoScore x
                    Nothing -> putStrLnE "Wrong input should be a positive number." >> getMaxScore
    echoScore :: Int -> IO Int
    echoScore sl = putStrLnA ("Score limit chosen: " ++ show sl) >> return sl

updateGameByPlayerAction :: Game -> Map PlayerName GamePlayer -> PlayerName -> PlayerAction -> IO Game
updateGameByPlayerAction game gamePlayers pn pa =
    let game' = applyPlayerAction game pn pa
     in if pa `elem` [CallTichu, CallGrandTichu]
            then do
                putStrLnA ("Player " ++ show pn ++ " called " ++ if pa == CallTichu then "Tichu." else "Grand Tichu.")
                pa' <- getPlayerActionsByName game' gamePlayers pn
                return $ applyPlayerAction game' pn pa'
            else putStrLnA ("Player " ++ show pn ++ " played " ++ show pa ++ ".") >> return game'

display :: Game -> IO ()
display game =
    putStrLnQ
        "Game State:"
        >> printQI
            (gamePhase game)
        >> putStrLnQI ("Board: " ++ showList' (board game))
        >> putStrLnQI ("Hands: " ++ showMap (hands game))
        >> putStrLnQI ("Tricks: " ++ showMap (tricks game))
        >> putStrLnQI ("Score: " ++ showMap (scores game))
        >> newLine

update :: Map PlayerName GamePlayer -> Game -> IO Game
update gamePlayers game = display game >> updateGame game gamePlayers

play :: Game -> Map PlayerName GamePlayer -> IO Game
play game gamePlayers =
    foldM
        (\g pn -> getPlayerActionsByName g gamePlayers pn >>= updateGameByPlayerAction g gamePlayers pn)
        game
        (sortBy currentPlayerFirst (getActivePlayers game))
  where
    currentPlayerFirst :: PlayerName -> PlayerName -> Ordering
    currentPlayerFirst pn' pn'' = case gamePhase game of
        Playing currentPlayer _ -> if pn' == currentPlayer then LT else if pn'' == currentPlayer then GT else EQ
        _ -> EQ

finish :: Game -> IO Game
finish game = do
    let winningTeams = Map.toList $ Map.filter (>= scoreLimit (gameConfig game)) (scores game)
    printWinners winningTeams
    return game
  where
    printWinners :: [(TeamName, Score)] -> IO ()
    printWinners [] = error "No team won!"
    printWinners [(winner, winningScore)] =
        putStrLnQ ("Yeay we have a winner! Team " ++ show winner ++ " won with a score of " ++ show winningScore ++ " points.")
    printWinners winners = putStrLnQ ("Yeeeey, we have more than one winner! The teams " ++ showList' winners)

getGameConfig :: IO GameConfig
getGameConfig = do
    players <- getPlayers
    teams <- getTeamNames
    GameConfig players teams <$> getMaxScore

getPlayerActions :: Game -> Map PlayerName GamePlayer -> IO (Map PlayerName PlayerAction)
getPlayerActions game gamePlayers = do
    let players = playerListWithCurrentPlayerFirst game
    actions <-
        mapM (getPlayerActionsByName game gamePlayers) players
    return $ Map.fromList $ zip players actions

getPlayerActionsByName :: Game -> Map PlayerName GamePlayer -> PlayerName -> IO PlayerAction
getPlayerActionsByName game gamePlayers pn =
    let allPossibleActions = possiblePlayerActions game pn
        gamePlayer = gamePlayers Map.! pn
     in gamePlayer game allPossibleActions pn

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

distribute :: Game -> IO Game
distribute game = return game{gamePhase = Playing (startingPlayer $ hands game) 0} -- TODO: Implement

updateGame :: Game -> Map PlayerName GamePlayer -> IO Game
updateGame game gamePlayers = do
    case gamePhase game of
        Starting -> startGame game
        Dealing _ -> return $ dealAllCards game
        Distributing -> distribute game
        Playing _ _ -> play game gamePlayers
        NextRound -> nextRound game
        GiveAwayLooserTricksAndHands -> return $ giveAwayLooserTricksAndHands game
        Scoring -> return $ score game
        Finished -> finish game
