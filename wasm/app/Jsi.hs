{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Jsi (module Jsi) where

import Bots.Random
import Cli
import CommandLinePlayer
import qualified Data.Aeson as AS
import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Wasm.Prim (JSString)
import qualified GHC.Wasm.Prim as WP

import qualified Data.Map as Map
import Game.Structures
import Game.Tichu
import Game.Utils
import IO

deriving instance Generic Value
deriving instance Generic Color
deriving instance Generic TichuCard
deriving instance Generic TichuCombination
deriving instance Generic GameConfig
deriving instance Generic GamePhase
deriving instance Generic TichuType
deriving instance Generic PlayerAction
deriving instance Generic Game

instance FromJSON Value
instance ToJSON Value

instance FromJSON Color
instance ToJSON Color

instance FromJSON TichuCard
instance ToJSON TichuCard

instance FromJSON TichuCombination
instance ToJSON TichuCombination

instance FromJSON GameConfig
instance ToJSON GameConfig

instance FromJSON GamePhase
instance ToJSON GamePhase

instance FromJSON TichuType
instance ToJSON TichuType

instance FromJSON PlayerAction
instance ToJSON PlayerAction

instance FromJSON Game
instance ToJSON Game

toJsString :: (ToJSON a) => a -> JSString
toJsString = WP.toJSString . BL.unpack . AS.encode

fromJsString :: (FromJSON a) => JSString -> Maybe a
fromJsString = AS.decode . BL.pack . WP.fromJSString

updateTichu :: JSString -> IO JSString
updateTichu js_game =
    case fromJsString js_game of
        Just g -> do game' <- updateTichu' g; return $ toJsString game'
        Nothing -> return $ toJsString "ERROR: Wrong game state."

updateTichu' :: Game -> IO Game
updateTichu' game =
    let pln = playerNames' game
     in update (Map.fromList $ zip pln (map snd defaultPlayerTypes)) game

setupTichu :: IO JSString
setupTichu = fmap toJsString setupTichu'

setupTichu' :: IO Game
setupTichu' = do
    gameConf <- getGameConfig
    gamePlayers <- getGamePlayers $ sittingOrder gameConf
    let game = newGame gameConf
    return game
