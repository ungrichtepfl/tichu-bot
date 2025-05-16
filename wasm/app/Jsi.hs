{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Jsi (updateGame, newGame) where

import Bots.Random
import qualified Data.Aeson as AS
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Wasm.Prim (JSString)
import qualified GHC.Wasm.Prim as WP

import qualified Data.Map as Map
import Game.Structures
import qualified Game.Tichu as Tichu

import Data.Aeson (FromJSON (..), ToJSON (..), withArray)
import Data.Aeson.Types (Parser)  
import qualified Data.Aeson as Aeson
import Data.Bits (xor)
import Data.Scientific (toBoundedInteger)
import Data.Vector (fromList, toList)
import qualified Data.Vector as V
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Random (StdGen, genWord64, initStdGen, mkStdGen, split)

deriving instance Generic Value
deriving instance Generic Color
deriving instance Generic TichuCard
deriving instance Generic TichuCombination
deriving instance Generic GameConfig
deriving instance Generic GamePhase
deriving instance Generic TichuType
deriving instance Generic PlayerAction
deriving instance Generic Game

instance ToJSON StdGen where
    toJSON g = Aeson.toJSON (take 2 (genWords g))
      where
        genWords :: StdGen -> [Word64]
        genWords gen0 =
            let (w1, gen1) = genWord64 gen0
                (w2, _) = genWord64 gen1
             in [w1, w2]

instance FromJSON StdGen where
    parseJSON = withArray "StdGen" $ \arr -> do
        case toList arr of
            [v1, v2] -> do
                w1 <- parseWord64 v1
                w2 <- parseWord64 v2
                pure (mkStdGen (fromIntegral (w1 `xor` w2)))
            _ -> fail "Expected array of two elements"
      where
        parseWord64 :: Aeson.Value -> Parser Word64
        parseWord64 v = do
            n <- Aeson.parseJSON v
            maybe (fail "Invalid Word64") pure (toBoundedInteger n)

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

makeJsString :: (ToJSON a) => a -> JSString
makeJsString = WP.toJSString . BL.unpack . AS.encode

makeHsType :: (FromJSON a) => JSString -> Maybe a
makeHsType = AS.decode . BL.pack . WP.fromJSString

updateGame :: JSString -> JSString -> JSString
updateGame (makeHsType -> Just game) (makeHsType -> Just playersAction) =
    let
        output = Tichu.updateGame game playersAction
     in
        makeJsString output
-- TODO: Print the format of the Json string that is expected
updateGame jsGame jsPlayersAction =
    error $
        "ERROR: Wrong game state or players action.\nGameState: "
            ++ WP.fromJSString jsGame
            ++ "\nPlayers Actions: "
            ++ WP.fromJSString jsPlayersAction

newGame :: JSString -> JSString
newGame (makeHsType -> Just gameConfig) = makeJsString $ Tichu.newGame gameConfig 0
-- TODO: Print the format of the Json string that is expected
newGame jsGameConfig = error $ "ERROR: Wrong game config type " ++ WP.fromJSString jsGameConfig
