{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Jsi (updateGame, newGame) where

import Bots.Random

import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Wasm.Prim (JSString)
import qualified GHC.Wasm.Prim as WP

import qualified Data.Map as Map
import Game.Structures
import qualified Data.Aeson as AS
import Data.Aeson (FromJSON , ToJSON )
import qualified Game.Tichu as Tichu

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
