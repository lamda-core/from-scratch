module ParserTests where

import Parser
import Test.Hspec

parserTests :: SpecWith ()
parserTests = describe "--== Parser ==--" $ do
  let parse' :: String -> Parser a -> Either String a
      parse' source parser = case parse source parser of
        Left (Error message _) -> Left message
        Right x -> Right x

  describe "☯ Control flow" $ do
    it "☯ succeed" $ do
      parse' "abc" (succeed True) `shouldBe` Right True

    it "☯ expected" $ do
      parse' "abc" (expected "something" :: Parser ()) `shouldBe` Left "something"

    it "☯ fmap" $ do
      parse' "abc" (fmap not (succeed True)) `shouldBe` Right False

    it "☯ orElse" $ do
      parse' "abc" (succeed True |> orElse (succeed False)) `shouldBe` Right True
      parse' "abc" (expected "something" |> orElse (succeed False)) `shouldBe` Right False

  describe "☯ Single characters" $ do
    it "☯ anyChar" $ do
      parse' "abc" anyChar `shouldBe` Right 'a'
      parse' "" anyChar `shouldBe` Left "a character"

    it "☯ space" $ do
      parse' " " space `shouldBe` Right ' '
      parse' "\t" space `shouldBe` Right '\t'
      parse' "\n" space `shouldBe` Right '\n'
      parse' "\r" space `shouldBe` Right '\r'
      parse' "\f" space `shouldBe` Right '\f'
      parse' "\v" space `shouldBe` Right '\v'
      parse' "a" space `shouldBe` Left "a blank space"

    it "☯ letter" $ do
      parse' "a" letter `shouldBe` Right 'a'
      parse' "A" letter `shouldBe` Right 'A'
      parse' " " letter `shouldBe` Left "a letter"

    it "☯ lower" $ do
      parse' "a" lower `shouldBe` Right 'a'
      parse' "A" lower `shouldBe` Left "a lowercase letter"

    it "☯ upper" $ do
      parse' "A" upper `shouldBe` Right 'A'
      parse' "a" upper `shouldBe` Left "an uppercase letter"

    it "☯ digit" $ do
      parse' "0" digit `shouldBe` Right '0'
      parse' "a" digit `shouldBe` Left "a digit from 0 to 9"

    it "☯ alphanumeric" $ do
      parse' "0" alphanumeric `shouldBe` Right '0'
      parse' "a" alphanumeric `shouldBe` Right 'a'
      parse' " " alphanumeric `shouldBe` Left "a letter or digit"

    it "☯ punctuation" $ do
      parse' "." punctuation `shouldBe` Right '.'
      parse' "?" punctuation `shouldBe` Right '?'
      parse' " " punctuation `shouldBe` Left "a punctuation character"

    it "☯ char" $ do
      parse' "a" (char 'a') `shouldBe` Right 'a'
      parse' "A" (char 'a') `shouldBe` Right 'A'
      parse' " " (char 'a') `shouldBe` Left "the character 'a'"

    it "☯ charCaseSensitive" $ do
      parse' "a" (charCaseSensitive 'a') `shouldBe` Right 'a'
      parse' "A" (charCaseSensitive 'a') `shouldBe` Left "the character 'a' (case sensitive)"

  describe "☯ Sequences" $ do
    it "☯ zeroOrOne" $ do
      parse' "abc!" (zeroOrOne letter) `shouldBe` Right ['a']
      parse' "_bc!" (zeroOrOne letter) `shouldBe` Right []

    it "☯ zeroOrMore" $ do
      parse' "abc!" (zeroOrMore letter) `shouldBe` Right ['a', 'b', 'c']
      parse' "_bc!" (zeroOrMore letter) `shouldBe` Right []

    it "☯ oneOrMore" $ do
      parse' "abc!" (oneOrMore letter) `shouldBe` Right ['a', 'b', 'c']
      parse' "_bc!" (oneOrMore letter) `shouldBe` Left "a letter"

    it "☯ chain" $ do
      parse' "_A5" (chain [] :: Parser [()]) `shouldBe` Right []
      parse' "_A5" (chain [char '_', letter, digit]) `shouldBe` Right ['_', 'A', '5']
