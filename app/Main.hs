module Main where

import           Control.Monad
import           Lib
import           System.IO
import           Text.Read     (readMaybe)

-- Formulate the output text for a given chain of numbers
response :: (Show a, Eq a, Num a) => [a] -> String
response [4]    = "4 is the magic number~"
response [x]    = show x ++ "is the magic number?" -- this one shouldn't happen
response [x, y] = show x ++ " is " ++ show y ++ ", and " ++ response [y]
response (x:xs) = show x ++ " is " ++ response xs

-- Get user input with an inline prompt, similar to ghci
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

main :: IO ()
main = do
  putStrLn "Welcome to the magic number game! Enter your numbers below:"
  forever $ do
    line <- prompt "\n > "
    case readMaybe line of
      Just n  -> putStrLn . response . chain $ n
      Nothing -> putStrLn "That number didn't work! Was it an integer?"
