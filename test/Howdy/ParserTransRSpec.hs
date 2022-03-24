module Howdy.ParserTransRSpec where

import qualified Data.Text             as T
import           Data.Text.Arbitrary   ()
import           Howdy.Internal.Parser.TransR
import Howdy.Internal.Parser.ConsR
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
    describe "Howdy.Parser.TransR" $ do
        describe "char" $ do
            prop "parses a matching character from an input" $
                \x xs -> runParser (char x) (T.cons x xs) `shouldBe` Success x xs
            prop "fails when the input doesn't match" $
                \x xs -> runParser (char x) xs `shouldBe` Fail xs

        describe "notChar" $ do
            prop "parses a non-matching character from an input" $
                \x y ys -> runParser (notChar x) (T.cons y ys) `shouldBe` Success y ys
            prop "fails when the input matches" $
                \x xs -> runParser (notChar x) (T.cons x xs) `shouldBe` Fail xs
            prop "fails when given an empty input" $
                \x -> runParser (notChar x) "" `shouldBe` Fail ""

        describe "anyChar" $ do
            it "parses the first character from an input" $ do
                runParser anyChar "abcd" `shouldBe` Success 'a' "bcd"
                runParser anyChar "bbcd" `shouldBe` Success 'b' "bcd"
            it "fails when given an empty input" $ do
                runParser anyChar "" `shouldBe` Fail ""

        describe "chars" $ do
            it "parses one of a list of matching characters" $ do
                runParser (chars "ab") "acde" `shouldBe` Success 'a' "cde"
                runParser (chars "ab") "bcde" `shouldBe` Success 'b' "cde"
            it "fails when given an empty input" $ do
                runParser (chars "ab") "" `shouldBe` Fail ""
            it "fails when given an empty list" $ do
                runParser (chars []) "abcd" `shouldBe` Fail "abcd"

        describe "string" $ do
            it "parses an exact string as text from an input" $ do
                runParser (string "hello") "hello world" `shouldBe` Success "hello" " world"
            it "fails when given a non-matching input" $ do
                runParser (string "moi") "hello world" `shouldBe` Fail "hello world"
                runParser (string "hola") "hello" `shouldBe` Fail "hello"
            it "always matches the empty string" $ do
                runParser (string "") "anything here" `shouldBe` Success "" "anything here"

        describe "text" $ do
            it "parses all characters as text until a predicate passes" $ do
                runParser (text (== ',')) "hello, world" `shouldBe` Success "hello" ", world"
            it "parses the whole input when the character never matches" $ do
                runParser (text (== ',')) "hello world" `shouldBe` Success "hello world" ""
            it "fails when the character is the first in the input" $ do
                runParser (text (== 'a')) "abcd" `shouldBe` Fail "abcd"
            it "fails when given an empty input" $ do
                runParser (text (== 'a')) "" `shouldBe` Fail ""

        describe "word" $ do
            it "parses all characters as text until a space character" $ do
                runParser word "hello world" `shouldBe` Success "hello" " world"
            it "ignores spaces before the word" $ do
                runParser word " hello world" `shouldBe` Success "hello" " world"

        describe "flag" $ do
            it "parses a word preceded by \"--\"" $ do
                runParser flag "--flag test" `shouldBe` Success "flag" " test"

        describe "rest" $ do
            it "parses the whole input" $ do
                runParser rest "abcd" `shouldBe` Success "abcd" ""
            it "returns an empty result when given an empty input" $ do
                    runParser rest "" `shouldBe` Success "" ""

        describe "firstof" $ do
            it "returns the result of the first successful parser for a given input" $ do
                runParser (firstof string ["aa", "bb", "ab"]) "abbaacde" `shouldBe` Success "ab" "baacde"
