module Howdy.ParserSpec where

import Howdy.Parser
import Test.Hspec

spec :: Spec
spec = do
    describe "Howdy.Parser" $ do
        describe "char" $ do
            it "parses a matching character from an input" $ do
                runParser (char 'a') "abcd" `shouldBe` Just ('a', "bcd")
            it "fails when the input doesn't match" $ do
                runParser (char 'b') "abcd" `shouldBe` Nothing

        describe "notChar" $ do
            it "parses a non-matching character from an input" $ do
                runParser (notChar 'a') "bcde" `shouldBe` Just ('b', "cde")
            it "fails when the input matches" $ do
                runParser (notChar 'b') "bcde" `shouldBe` Nothing
            it "fails when given an empty input" $ do
                runParser (notChar 'a') "" `shouldBe` Nothing

        describe "anyChar" $ do
            it "parses the first character from an input" $ do
                runParser anyChar "abcd" `shouldBe` Just ('a', "bcd")
                runParser anyChar "bbcd" `shouldBe` Just ('b', "bcd")
            it "fails when given an empty input" $ do
                runParser anyChar "" `shouldBe` Nothing

        describe "chars" $ do
            it "parses one of a list of matching characters" $ do
                runParser (chars "ab") "acde" `shouldBe` Just ('a', "cde")
                runParser (chars "ab") "bcde" `shouldBe` Just ('b', "cde")
            it "fails when given an empty input" $ do
                runParser (chars "ab") "" `shouldBe` Nothing
            it "fails when given an empty list" $ do
                runParser (chars []) "abcd" `shouldBe` Nothing

        describe "string" $ do
            it "parses an exact string as text from an input" $ do
                runParser (string "hello") "hello world" `shouldBe` Just ("hello", " world")
            it "fails when given a non-matching input" $ do
                runParser (string "moi") "hello world" `shouldBe` Nothing
                runParser (string "hola") "hello" `shouldBe` Nothing
            it "always matches the empty string" $ do
                runParser (string "") "anything here" `shouldBe` Just ("", "anything here")

        describe "text" $ do
            it "parses all characters as text until a specific character" $ do
                runParser (text ',') "hello, world" `shouldBe` Just ("hello", ", world")
            it "parses the whole input when the character never matches" $ do
                runParser (text ',') "hello world" `shouldBe` Just ("hello world", "")
            it "returns an empty text when the character is the first in the input" $ do
                runParser (text 'a') "abcd" `shouldBe` Just ("", "abcd")
            it "returns an empty result when given an empty input" $ do
                runParser (text 'a') "" `shouldBe` Just ("", "")

        describe "word" $ do
            it "parses all characters as text until a space character" $ do
                runParser word "hello world" `shouldBe` Just ("hello", " world")
            it "ignores spaces before the word" $ do
                runParser word " hello world" `shouldBe` Just ("hello", " world")

        describe "flag" $ do
            it "parses a word preceded by \"--\"" $ do
                runParser flag "--flag test" `shouldBe` Just ("flag", " test")

        describe "rest" $ do
            it "parses the whole input" $ do
                runParser rest "abcd" `shouldBe` Just ("abcd", "")
            it "returns an empty result when given an empty input" $ do
                    runParser rest "" `shouldBe` Just ("", "")

        describe "firstof" $ do
            it "returns the result of the first successful parser for a given input" $ do
                runParser (firstof string ["aa", "bb", "ab"]) "abbaacde" `shouldBe` Just ("ab", "baacde")