module Help.HelpSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List

import Help.Command
import Help.Printer

spec :: Spec
spec = do
  describe "Modulo Help" $ do
      context "Show Instance" $ do
        it "should print the object" $ do
            show $ Command "my-application" [OptionGroup "Test" [Single "--ef" "Exclude file"]
                                             ,OptionGroup "Another" [Single "--exts" "Only extensions"]]
            `shouldBe`  "my-application [Test - [--ef - Exclude file],Another - [--exts - Only extensions]]"

      context "Usage" $ do
          context "Single Option" $ do
            it "Should return a formatted string with command usage information" $ do
                usage $ Command "my-application" [OptionGroup "Test" [Single "--ef" "Exclude file"]
                                                  ,OptionGroup "Test" [Single "--exts" "Only extensions"]]
                `shouldBe` "my-application [[--ef]] [[--exts]]\n\n"

          context "Extended Option" $ do
            it "Should return a formatted string with command usage information" $ do
                usage $ Command "my-application" [OptionGroup "Test" [Extended ["--ef", "--exclude-files"] "Exclude file"], OptionGroup "Test" [Extended ["--exts", "--only-extensions"] "Only extensions"]]
                `shouldBe` "my-application [[--ef]] [[--exts]]\n\n"

          context "Mixed Option" $ do
            it "Should return a formatted string with command usage information" $ do
                usage $ Command "my-application" [OptionGroup "Test" [Extended ["--ef", "--exclude-files"] "Exclude file"]
                                                  ,OptionGroup "Test"  [Single "--exts" "Only extensions"]]
                `shouldBe` "my-application [[--ef]] [[--exts]]\n\n"

          context "Several Options" $ do
            it "Should return a formatted string with command usage information" $ do
                usage hsCommand `shouldBe` "my-application [[some-text]] [[--ed][--ef][--exts]] [[--p][--pc][--cmd]]\n\n"

      context "Printer" $ do
            it "should print the description of all commands " $ do
                printHelp (TwoColumns (40,100)) hsCommand
                `shouldSatisfy` (\x -> isInfixOf "Some description 3" x)

hsCommand = Command "my-application" [OptionGroup "main-group" [FixedText "[some-text]"]
                                     ,OptionGroup "other-group"  aOptionList
                                     ,OptionGroup "another-one"  anotherOptionList]

aOptionList :: [Option]
aOptionList = [Extended ["--ed", "--exclude-directories"]
                         "Some description"
              ,Extended ["--ef", "--exclude-files"]
                         "Some description 2"
              ,Extended ["--exts", "--only-extensions"]
                         "Some description 3"]

anotherOptionList :: [Option]
anotherOptionList = [Extended ["--p", "--print"]
                              "Some description 4"
                    ,Extended ["--pc", "--print-changed"]
                              "Some description 5"
                    ,Extended ["--cmd", "--command"]
                              "Some description 6"]