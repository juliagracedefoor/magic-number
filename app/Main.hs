module Main where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Lib
import           System.IO
import           System.Exit
import           Text.Read

data Action = Quit | DoNothing | Respond

main :: IO ()
main = do
  putStrLn
    "Welcome to the magic number game! You can type 'quit' to quit. Enter your numbers below:"
  forever loop

loop :: IO ()
loop = do
  line <- prompt "\n > "
  let respondTo =
        putStrLn
          . maybe "That number didn't work! Are you sure it was an integer?"
                  (chainResponse . chain)
          . readMaybe

  case findIntent line of
    Quit      -> exitSuccess
    DoNothing -> return ()
    Respond   -> respondTo line

-- Get user input with an inline prompt, similar to ghci
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- Figure out what the user input wants the program to do
findIntent :: String -> Action
findIntent str | null str         = DoNothing
               | low == "q"       = Quit
               | low == "quit"    = Quit
               | low == "exit"    = Quit
               | low == "leave"   = Quit
               | low == "goodbye" = Quit
               | otherwise        = Respond
 where
  low       = map toLower firstWord
  firstWord = fromMaybe "" . listToMaybe . words $ str

-- Formulate the output text for a given chain of numbers
chainResponse :: (Show a, Eq a, Num a) => [a] -> String
chainResponse [4] = "4 is the magic number~"
chainResponse [x] =
  show x ++ "is the magic number? something may have gone wrong here"
chainResponse [x, y] =
  show x ++ " is " ++ show y ++ ", and " ++ chainResponse [y]
chainResponse (x : xs) = show x ++ " is " ++ chainResponse xs
