module Utils (module Utils) where

import           Control.Monad (forM)
import           Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           System.Random (randomRIO)

setEmpty :: Map k [a] -> Map k [a]
setEmpty = Map.map (const [])

setFalse :: Map k Bool -> Map k Bool
setFalse = Map.map (const False)

setNothing :: Map k (Maybe a) -> Map k (Maybe a)
setNothing = Map.map (const Nothing)

shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray' n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
 where
  n = length xs
  newArray' :: Int -> [a] -> IO (IOArray Int a)
  newArray' n' = newListArray (1, n')
