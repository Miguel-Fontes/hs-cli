module Utils.StringSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Utils.String


spec :: Spec
spec = do
  describe "Modulo String" $ do
    context "rtrim" $ do
      it "should remove all spaces at the end of the string" $ do
        rtrim "string teste    " `shouldBe` "string teste"

      it "should let any other spaces of the string untouuched" $ do
        rtrim "  string teste" `shouldBe` "  string teste"

    context "rpad" $ do
      it "should fill the string with spaces to the right" $ do
        rpad 10 "a" `shouldBe` "a         "

      it "should not change a string with length equal or greater than the targed length" $ do
        rpad 10 "1234567890" `shouldBe` "1234567890"

      it "should return a string with the target length" $ do
        length (rpad 10 "a") `shouldBe` 10

    context "lpad" $ do
      it "should fill the string with spaces to the right" $ do
        lpad 10 "a" `shouldBe` "         a"

      it "should not change a string with length equal or greater than the targed length" $ do
        lpad 10 "1234567890" `shouldBe` "1234567890"

      it "should return a string with the target length" $ do
        length (lpad 10 "a") `shouldBe` 10

    context "margin" $ do
      it "should return a string of whitespaces" $ do
        margin 10 `shouldBe` "          "

    context "identation" $ do
      it "should return the correct number of whitespaces for a level of identation " $ do
        identation 2 `shouldBe` "        "