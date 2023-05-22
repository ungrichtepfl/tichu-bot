{-# LANGUAGE LambdaCase #-}

-- Useful debug infos:
-- Enabling assertions: https://stackoverflow.com/questions/45777703/turning-on-assertions-while-compiling-with-haskells-stack-build-system
--  i.e. stack clean && stack build --fast && stack run
-- Profil build: https://stackoverflow.com/questions/32123475/profiling-builds-with-stack
--  i.e. stack run --profile -- +RTS -xc

module Tichu (module Tichu) where

import           Combinations
import           Constants
import           Control.Exception (assert)
import           Control.Monad     (foldM)
import           Data.List         (elemIndex, foldl', nub, sort, sortBy, (\\))
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Maybe        (fromJust, isJust, isNothing)
import           GamePlayer
import           IO
import           Structures
import           Text.Read         (readMaybe)
import           Utils

type GamePlayers = Map PlayerName GamePlayer

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

getGameConfig :: IO GameConfig
getGameConfig = do
  players <- getPlayers
  teams <- getTeamNames
  GameConfig players teams <$> getMaxScore

getGamePlayers :: [PlayerName] -> IO GamePlayers
getGamePlayers pns =
  putStrLnQ ("Enter player types separated by spaces for players " ++ showList' pns ++ " (default: " ++ showList' defaultPlayerTypes ++ ").")
    -- TODO: List possible options
    >> getTrimmedLine
    >>= processInput
 where
  processInput :: String -> IO GamePlayers
  processInput rawInput
    | rawInput == quitSymbol = exitGame
    | otherwise =
        let
          playerTypesRaw = words rawInput
         in
          case playerTypesRaw of
            [] -> echoPlayerTypes (Map.fromList $ zip pns defaultPlayerTypes)
            [_, _, _, _] -> case mapM read playerTypesRaw of
              Nothing -> putStrLnE "Wrong kind of player type." >> getGamePlayers pns -- TODO: List possible options
              Just pts -> echoPlayerTypes (Map.fromList $ zip pns pts)
            _ -> putStrLnE "Wrong number of player types, should be 4." >> getGamePlayers pns
  echoPlayerTypes :: GamePlayers -> IO GamePlayers
  echoPlayerTypes gps = putStrLnA ("Player types chosen: " ++ showMap gps) >> return gps

getPlayers :: IO [PlayerName]
getPlayers =
  putStrLnQ ("Enter player names separated by spaces (default: " ++ showList' defaultPlayerNames ++ "). This will also be the sitting order:")
    >> getTrimmedLine
    >>= processInput
 where
  processInput :: String -> IO [PlayerName]
  processInput rawInput
    | rawInput == quitSymbol = exitGame
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

playerListWithCurrentPlayerFirst :: Game -> [PlayerName]
playerListWithCurrentPlayerFirst game =
  let playerList = getActivePlayers game
      i = fromJust $ elemIndex (getCurrentPlayer game) playerList
   in drop i playerList ++ take i playerList

getPlayerActions :: Game -> GamePlayers -> IO (Map PlayerName PlayerAction)
getPlayerActions game gamePlayers = do
  let players = playerListWithCurrentPlayerFirst game
  actions <-
    mapM (getPlayerActionsByName game gamePlayers) players
  return $ Map.fromList $ zip players actions

getPlayerActionsByName :: Game -> GamePlayers -> PlayerName -> IO PlayerAction
getPlayerActionsByName game gamePlayers pn =
  let allPossibleActions = possiblePlayerActions game pn
      gamePlayer = gamePlayers Map.! pn
   in pickPlayerAction gamePlayer game allPossibleActions pn

canStillCallTichu :: Game -> PlayerName -> Bool
canStillCallTichu game pn = length (hands game Map.! pn) == maxCards && isNothing (tichus game Map.! pn)

update :: Game -> GamePlayers -> IO Game
update game gamePlayers = do
  case gamePhase game of
    Starting                     -> startGame game
    Dealing _                    -> return $ dealAllCards game
    Distributing                 -> distribute game
    Playing _ _                  -> play game gamePlayers
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

play :: Game -> GamePlayers -> IO Game
play game gamePlayers =
  foldM (\g pn -> getPlayerActionsByName g gamePlayers pn >>= updateGameByPlayerAction g gamePlayers pn) game (sortBy currentPlayerFirst (getActivePlayers game))
 where
  currentPlayerFirst :: PlayerName -> PlayerName -> Ordering
  currentPlayerFirst pn' pn'' = case gamePhase game of
    Playing currentPlayer _ -> if pn' == currentPlayer then LT else if pn'' == currentPlayer then GT else EQ
    _ -> EQ

updateGameByPlayerAction :: Game -> GamePlayers -> PlayerName -> PlayerAction -> IO Game
updateGameByPlayerAction game gamePlayers pn pa =
  let game' = applyPlayerAction game pn pa
   in if pa `elem` [CallTichu, CallGrandTichu]
        then do
          putStrLnA ("Player " ++ show pn ++ " called " ++ if pa == CallTichu then "Tichu." else "Grand Tichu.")
          pa' <- getPlayerActionsByName game' gamePlayers pn
          return $ applyPlayerAction game' pn pa'
        else putStrLnA ("Player " ++ show pn ++ " played " ++ show pa ++ ".") >> return game'

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
    >> putStrLnQI ("Board: " ++ showList' (board game))
    >> putStrLnQI ("Hands: " ++ showMap (hands game))
    >> putStrLnQI ("Tricks: " ++ showMap (tricks game))
    >> putStrLnQI ("Score: " ++ showMap (scores game))

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
  printWinners winners = putStrLnQ ("Yeeeey, we have more than one winner! The teams " ++ showList' winners)

run :: GamePlayers -> Game -> IO Game
run gamePlayers game = display game >> update game gamePlayers

playTichu :: IO ()
playTichu = do
  gameConf <- getGameConfig
  gamePlayers <- getGamePlayers $ sittingOrder gameConf
  iterateUntilM_ shouldGameStop (run gamePlayers) (newGame gameConf)
