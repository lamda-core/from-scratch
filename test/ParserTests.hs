module ParserTests where

import Parser
import Test.Hspec

parserTests :: SpecWith ()
parserTests = describe "--== Parser ==--" $ do
  let error :: String -> Parser a -> Maybe String
      error text parser = case parse text parser of
        Left (Expected message _) -> Just message
        _ -> Nothing

  it "☯ anyChar" $ do
    parse "abc" anyChar `shouldBe` Right 'a'
    error "" anyChar `shouldBe` Just "a character"

  it "☯ char" $ do
    parse "abc" (char 'a') `shouldBe` Right 'a'
    error "xbc" (char 'a') `shouldBe` Just "the character 'a'"
