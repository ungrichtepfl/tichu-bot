module Game.Interface (module Game.Interface) where

import Data.Map (Map)

import Game.Structures

class Interface a where
    gameInit :: a -> Int -> IO ()
    gameDeinit :: a -> IO ()
    windowShouldClose :: a -> IO Bool
    updateDrawConfig :: a -> IO (Maybe GameConfig)
    getUserAction :: a -> IO (Maybe PlayerAction)
    updateStateAndRenderGame :: a -> (Game, Maybe (Map PlayerName [PlayerAction])) -> IO ()
