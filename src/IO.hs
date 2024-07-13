module IO (module IO) where

import Data.Char (isSpace)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

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

printQ :: Show a => a -> IO ()
printQ = putStrLnQ . show

printA :: Show a => a -> IO ()
printA = putStrLnA . show

printQI :: Show a => a -> IO ()
printQI = putStrLnQI . show

printE :: Show a => a -> IO ()
printE = putStrLnE . show

trim :: String -> String
trim =
 let
  f = reverse . dropWhile isSpace
  in
  f . f

printPrompt :: IO ()
printPrompt = putStr inputPrompt

getLineFlushed :: IO String
getLineFlushed = hFlush stdout >> getLine

getTrimmedLine :: IO String
getTrimmedLine = printPrompt >> (trim <$> getLineFlushed)

printWithNumber :: Show a => Int -> a -> IO Int
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
