module Main where

import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "wordify" $ do

    it "turns integers into english numerals" $
      wordify 4 `shouldBe` "four"

    it "works with negative numbers" $
      wordify (-3) `shouldBe` "negative three"

  describe "countLetters" $ do

    it "counts the number of letters in a word" $
      countLetters "hello" `shouldBe` 5

    it "only counts characters that are alphabetical" $
      countLetters "wi-fi1234" `shouldBe` 4

  describe "chain" $ do

    it "derives the list of numbers properly" $
      chain 11 `shouldBe` [11, 6, 3, 5, 4]

    it "always ends in four" $
      property $ \x -> (last . chain $ x) == (4 :: Integer)
