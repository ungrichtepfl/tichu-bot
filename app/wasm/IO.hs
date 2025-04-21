module IO (module IO) where

import Data.Char (isSpace)
import Data.Map (Map)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import qualified Data.Map as Map

import Game.Structures
import Game.Utils

outputPromptChar :: Char
outputPromptChar = '>'

inputPromptChar :: Char
inputPromptChar = '$'

inputPrompt :: String
inputPrompt = inputPromptChar : " "

newLine :: IO ()
newLine = putStrLn ""

outputPrompt :: Int -> String
outputPrompt n = replicate n outputPromptChar ++ " "

putStrLnQ :: String -> IO ()
putStrLnQ = putStrLn . (outputPrompt 2 ++)

putStrLnA :: String -> IO ()
putStrLnA s = putStrLn (outputPrompt 1 ++ s) >> newLine

putStrLnQI :: String -> IO ()
putStrLnQI = putStrLn . (outputPrompt 4 ++)

putStrLnE :: String -> IO ()
putStrLnE = putStrLn . ("ERROR: " ++)

putStrLnD :: String -> IO ()
putStrLnD = putStrLn . ("DEBUG: " ++)

printQ :: (Show a) => a -> IO ()
printQ = putStrLnQ . show

printA :: (Show a) => a -> IO ()
printA = putStrLnA . show

printQI :: (Show a) => a -> IO ()
printQI = putStrLnQI . show

printE :: (Show a) => a -> IO ()
printE = putStrLnE . show

trim :: String -> String
trim =
    let f = reverse . dropWhile isSpace
     in f . f

printPrompt :: IO ()
printPrompt = putStr inputPrompt

getLineFlushed :: IO String
getLineFlushed = hFlush stdout >> getLine

getTrimmedLine :: IO String
getTrimmedLine = printPrompt >> (trim <$> getLineFlushed)

printWithNumber :: (Show a) => Int -> a -> IO Int
printWithNumber i x = putStrLnQI ("(" ++ show i ++ ") " ++ show x) >> return (i + 1)

selectFromList :: (Eq a) => [a] -> String -> IO (Maybe a)
selectFromList selectionList rawInput = do
    case readMaybe rawInput of
        Nothing ->
            putStrLnE "You must enter a number!"
                >> return Nothing
        Just selectionNumber ->
            if selectionNumber < 0 || selectionNumber >= length selectionList
                then
                    putStrLnE
                        ( "Invalid action number! Must be between 0 and "
                            ++ show (length selectionList - 1)
                            ++ "."
                        )
                        >> return Nothing
                else do
                    let selection = selectionList !! selectionNumber
                    if selection `elem` selectionList
                        then return $ Just selection
                        else
                            putStrLnE "Invalid action"
                                >> return Nothing

showListSep :: (Show a) => String -> [a] -> String
showListSep _ [] = ""
showListSep _ [x] = show x
showListSep sep (x : xs) = show x ++ sep ++ showListSep sep xs

showStringListSep :: String -> [String] -> String
showStringListSep _ [] = ""
showStringListSep _ [x] = x
showStringListSep sep (x : xs) = x ++ sep ++ showStringListSep sep xs

showCharListSep :: String -> [Char] -> String
showCharListSep _ [] = ""
showCharListSep _ [x] = [x]
showCharListSep sep (x : xs) = x : (sep ++ showCharListSep sep xs)

showMapSep :: (Show k, Show v) => String -> Map k v -> String
showMapSep sep m = showMapAsList $ Map.toList m
  where
    showMapAsList :: (Show k, Show v) => [(k, v)] -> String
    showMapAsList [] = ""
    showMapAsList [(k, v)] = show k ++ " -> " ++ show v
    showMapAsList ((k, v) : xs) = show k ++ " -> " ++ show v ++ sep ++ showMapAsList xs

showMapListSep :: (Show k, Show v) => String -> Map k [v] -> String
showMapListSep sep m = showMapAsList $ Map.toList m
  where
    showMapAsList :: (Show k, Show v) => [(k, [v])] -> String
    showMapAsList [] = ""
    showMapAsList [(k, v)] = show k ++ " -> " ++ showList' v
    showMapAsList ((k, v) : xs) = show k ++ " -> " ++ showList' v ++ sep ++ showMapAsList xs

showMapCharListSep :: (Show k) => String -> Map k [Char] -> String
showMapCharListSep sep m = showMapCharAsList $ Map.toList m
  where
    showMapCharAsList :: (Show k) => [(k, [Char])] -> String
    showMapCharAsList [] = ""
    showMapCharAsList [(k, v)] = show k ++ " -> " ++ showCharList v
    showMapCharAsList ((k, v) : xs) = show k ++ " -> " ++ showCharList v ++ sep ++ showMapCharAsList xs

showMapStringListSep :: (Show k) => String -> Map k [String] -> String
showMapStringListSep sep m = showMapCharAsList $ Map.toList m
  where
    showMapCharAsList :: (Show k) => [(k, [String])] -> String
    showMapCharAsList [] = ""
    showMapCharAsList [(k, v)] = show k ++ " -> " ++ showStringList v
    showMapCharAsList ((k, v) : xs) = show k ++ " -> " ++ showStringList v ++ sep ++ showMapCharAsList xs

showList' :: (Show a) => [a] -> String
showList' = showListSep ", "

showStringList :: [String] -> String
showStringList = showStringListSep ", "

showCharList :: [Char] -> String
showCharList = showCharListSep ", "

showMap :: (Show k, Show v) => Map k v -> String
showMap = showMapSep ", "

showMapList :: (Show k, Show v) => Map k [v] -> String
showMapList = showMapListSep ", "

showMapCharList :: (Show k) => Map k [Char] -> String
showMapCharList = showMapCharListSep ", "

showMapStringList :: (Show k) => Map k [String] -> String
showMapStringList = showMapStringListSep ", "

showLastPlayedCardsSep :: String -> Game -> String
showLastPlayedCardsSep sep game = case board game of
    [] -> "Empty board"
    (lastComb : _) -> showListSep sep $ cardsFromCombination lastComb

showLastPlayedCards :: Game -> String
showLastPlayedCards = showLastPlayedCardsSep ", "
