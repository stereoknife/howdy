module Howdy.CommandSpec where

import qualified Data.Text             as T
import           Data.Text.Arbitrary   ()
import           Howdy.Internal.Action.Command
import           Test.Hspec            (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import Data.Default (Default (def))

spec :: Spec
spec = do
    describe "Howdy.Internal.Command" $ do
        describe "alias" $ do
            it "sets the alias of a command" $ do
                build' (alias "alias") def `shouldBe` (def { alias = "alias" }, def)

            prop "multiple aliases are appended" $
                \x y -> build' (alias x >> alias y) def `shouldBe` build' (alias (x ++ y)) def

        describe "desc" $ do
            it "sets the description of a command" $ do
                todo
            prop "multiple descriptions are an error" $
                todo

        describe "legacy" $ do
            it "overrides the identifier hash" $
                command "a" (legacy "b") `shouldBe` (def { alias = ["a"], ident = (hash "b")}, def)

        describe "permission" $ do
            it "sets the permission thingy" $
                todo

        describe "run" $ do
            it "sets the runner" $
                todo