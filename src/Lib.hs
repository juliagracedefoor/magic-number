module Lib where

import           Data.Char                 (isLetter)
import           Data.List                 (genericLength)
import           Data.Maybe                (fromJust, isJust)
import           Text.Numeral.Grammar      (defaultInflection)

import qualified Data.Text                 as T
import qualified Text.Numeral.Language.ENG as EN

-- Convert a number into its English numeral form
wordify :: (Integral a) => a -> String
wordify n | isJust numeral = T.unpack . fromJust $ numeral
          | otherwise      = error "That number doesn't work for some reason"
    where numeral = EN.us_cardinal defaultInflection n

-- Count the number of alphabetical characters in a string
countLetters :: (Integral a) => String -> a
countLetters = genericLength . filter isLetter

-- Continue the chain until we reach the magic number
chain :: (Integral a) => a -> [a]
chain 4 = [4]
chain x = x : chain (countLetters . wordify $ x)
