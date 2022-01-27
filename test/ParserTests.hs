module ParserTests where

import Parser
import Test.Hspec

parserTests :: SpecWith ()
parserTests = describe "--== Parser ==--" $ do
  let error :: String -> Parser a -> Maybe String
      error source parser = case parse source parser of
        Left (Expected message _) -> Just message
        _ -> Nothing

  describe "☯ Single characters" $ do
    it "☯ anyChar" $ do
      parse "abc" anyChar `shouldBe` Right 'a'
      parse "" anyChar
        `shouldBe` Left
          ( Expected "a character" $
              State
                { source = "",
                  remaining = "",
                  lastChar = Nothing,
                  token = Token {name = "", row = 1, col = 1},
                  stack = []
                }
          )

    it "☯ charIf" $ do
      parse "abc" (charIf ('a' ==) "an 'a'") `shouldBe` Right 'a'
      parse "xbc" (charIf ('a' ==) "an 'a'")
        `shouldBe` Left
          ( Expected "an 'a'" $
              State
                { source = "xbc",
                  remaining = "bc",
                  lastChar = Just 'x',
                  token = Token {name = "", row = 1, col = 2},
                  stack = []
                }
          )

    it "☯ space" $ do
      parse " " space `shouldBe` Right ' '
      parse "\t" space `shouldBe` Right '\t'
      parse "\n" space `shouldBe` Right '\n'
      parse "\r" space `shouldBe` Right '\r'
      parse "\f" space `shouldBe` Right '\f'
      parse "\v" space `shouldBe` Right '\v'
      error "a" space `shouldBe` Just "a blank space"

    it "☯ letter" $ do
      parse "a" letter `shouldBe` Right 'a'
      parse "A" letter `shouldBe` Right 'A'
      error " " letter `shouldBe` Just "a letter"

    it "☯ letterLower" $ do
      parse "a" letterLower `shouldBe` Right 'a'
      error "A" letterLower `shouldBe` Just "a lowercase letter"

    it "☯ letterUpper" $ do
      parse "A" letterUpper `shouldBe` Right 'A'
      error "a" letterUpper `shouldBe` Just "an uppercase letter"

    it "☯ digit" $ do
      parse "0" digit `shouldBe` Right '0'
      error "a" digit `shouldBe` Just "a digit from 0 to 9"

    it "☯ alphanumeric" $ do
      parse "0" alphanumeric `shouldBe` Right '0'
      parse "a" alphanumeric `shouldBe` Right 'a'
      error " " alphanumeric `shouldBe` Just "a letter or digit"

    it "☯ punctuation" $ do
      parse "." punctuation `shouldBe` Right '.'
      parse "?" punctuation `shouldBe` Right '?'
      error " " punctuation `shouldBe` Just "a punctuation character"

    it "☯ char" $ do
      parse "a" (char 'a') `shouldBe` Right 'a'
      parse "A" (char 'a') `shouldBe` Right 'A'
      error " " (char 'a') `shouldBe` Just "the character 'a'"

    it "☯ charCaseSensitive" $ do
      parse "a" (charCaseSensitive 'a') `shouldBe` Right 'a'
      error "A" (charCaseSensitive 'a') `shouldBe` Just "the character 'a' (case sensitive)"

    it "☯ except" $ do
      parse "a" (except space) `shouldBe` Right 'a'
      error " " (except space) `shouldBe` Just "something else"

  describe "☯ Sequences" $ do
    it "☯ zeroOrOne" $ do
      parse "abc!" (zeroOrOne letter) `shouldBe` Right ['a']
      parse "_bc!" (zeroOrOne letter) `shouldBe` Right []

    it "☯ zeroOrMore" $ do
      parse "abc!" (zeroOrMore letter) `shouldBe` Right ['a', 'b', 'c']
      parse "_bc!" (zeroOrMore letter) `shouldBe` Right []

    it "☯ oneOrMore" $ do
      parse "abc!" (oneOrMore letter) `shouldBe` Right ['a', 'b', 'c']
      error "_bc!" (oneOrMore letter) `shouldBe` Just "a letter"

    it "☯ chain" $ do
      parse "_A5" (chain [char '_', letter, digit]) `shouldBe` Right ['_', 'A', '5']
