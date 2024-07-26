module Game.Constants (module Game.Constants) where

import Game.Structures

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

matchBonus :: Int
matchBonus = 200

tichuBonus :: Int
tichuBonus = 100

grandTichuBonus :: Int
grandTichuBonus = 200
