import qualified Data.Map as Map
import Test.Hspec
import Typed

main :: IO ()
main = hspec $ do
  describe "--== Typed ==--" $ do
    it "☯ defineType" $ do
      defineType "T" [] [] Map.empty `shouldBe` Map.singleton "T" (TTyp [])
      defineType "T" [TVar "a", TVar "b"] [] Map.empty `shouldBe` Map.singleton "T" (TFun (TVar "a") $ TFun (TVar "b") $ TTyp [])
      defineType "T" [] [("A", TVar "T"), ("B", TFun TInt (TVar "T"))] Map.empty
        `shouldBe` Map.fromList
          [ ("T", TTyp ["A", "B"]),
            ("A", TVar "T"),
            ("B", TFun TInt $ TVar "T")
          ]

    it "☯ occurs" $ do
      "x" `occurs` TTup `shouldBe` False
      "x" `occurs` TInt `shouldBe` False
      "x" `occurs` TVar "x" `shouldBe` True
      "x" `occurs` TVar "y" `shouldBe` False
      "x" `occurs` TTyp ["x"] `shouldBe` False
      "x" `occurs` TFun (TVar "y") (TVar "y") `shouldBe` False
      "x" `occurs` TFun (TVar "x") (TVar "y") `shouldBe` True
      "x" `occurs` TFun (TVar "y") (TVar "x") `shouldBe` True
      "x" `occurs` TRec [] `shouldBe` False
      "x" `occurs` TRec [("x", TVar "y")] `shouldBe` False
      "x" `occurs` TRec [("y", TVar "x")] `shouldBe` True
      "x" `occurs` TAnn "y" (TVar "y") `shouldBe` False
      "x" `occurs` TAnn "x" (TVar "y") `shouldBe` True
      "x" `occurs` TAnn "y" (TVar "x") `shouldBe` True
      "x" `occurs` TApp (TVar "y") (TVar "y") `shouldBe` False
      "x" `occurs` TApp (TVar "x") (TVar "y") `shouldBe` True
      "x" `occurs` TApp (TVar "y") (TVar "x") `shouldBe` True

    it "☯ bind" $ do
      bind "x" (TVar "y") TTup `shouldBe` TTup
      bind "x" (TVar "y") TInt `shouldBe` TInt
      bind "x" (TVar "y") (TTyp ["x"]) `shouldBe` TTyp ["x"]
      bind "x" (TVar "y") (TVar "x") `shouldBe` TVar "y"
      bind "x" (TVar "y") (TVar "z") `shouldBe` TVar "z"
      bind "x" (TVar "y") (TFun (TVar "x") (TVar "x")) `shouldBe` TFun (TVar "y") (TVar "y")
      bind "x" (TVar "y") (TRec [("x", TVar "x")]) `shouldBe` TRec [("x", TVar "y")]
      bind "x" (TVar "y") (TAnn "x" (TVar "z")) `shouldBe` TVar "y"
      bind "x" (TVar "y") (TApp (TVar "x") (TVar "x")) `shouldBe` TApp (TVar "y") (TVar "y")

    it "☯ unify" $ do
      let unify' a b = fmap (\s -> s (TVar "x")) (unify a b)
      unify' TInt (TTyp []) `shouldBe` Left (TypeMismatch TInt (TTyp []))
      unify' TInt TInt `shouldBe` Right (TVar "x")
      unify' (TVar "x") TInt `shouldBe` Right TInt
      unify' TInt (TVar "x") `shouldBe` Right TInt
      unify' (TVar "x") (TFun (TVar "x") TInt) `shouldBe` Left (TypeMismatch (TVar "x") (TFun (TVar "x") TInt))
      unify' (TFun (TVar "x") TInt) (TVar "x") `shouldBe` Left (TypeMismatch (TFun (TVar "x") TInt) (TVar "x"))
      unify' (TFun (TVar "x") TInt) (TFun (TTyp []) TInt) `shouldBe` Right (TTyp [])
      unify' (TFun TInt (TVar "x")) (TFun TInt (TTyp [])) `shouldBe` Right (TTyp [])
      unify' (TAnn "x" TInt) (TTyp []) `shouldBe` Left (TypeMismatch TInt (TTyp []))
      unify' (TAnn "x" TInt) TInt `shouldBe` Right TInt
      unify' TInt (TAnn "x" TInt) `shouldBe` Right TInt

    it "☯ intToName" $ do
      intToName 0 `shouldBe` ""
      intToName 1 `shouldBe` "a"
      intToName 26 `shouldBe` "z"
      intToName 27 `shouldBe` "aa"
      intToName 28 `shouldBe` "ba"

    it "☯ newTypeName" $ do
      newTypeName 24 TInt `shouldBe` "x"
      newTypeName 24 (TVar "x") `shouldBe` "y"
      newTypeName 24 (TFun (TVar "x") (TVar "y")) `shouldBe` "z"

    it "☯ check" $ do
      let check' expr env = fmap fst (check expr env)
      check' Tup Map.empty `shouldBe` Right TTup
      check' (Int 1) Map.empty `shouldBe` Right TInt
      check' (Var "x") Map.empty `shouldBe` Left (UndefinedName "x")
      check' (Var "x") (Map.singleton "x" TInt) `shouldBe` Right TInt
      check' (Var "x") (Map.singleton "x" (TVar "y")) `shouldBe` Right (TVar "y")
      check' (Ann Tup TInt) Map.empty `shouldBe` Left (TypeMismatch TInt TTup)
      check' (Ann Tup TTup) Map.empty `shouldBe` Right TTup
      check' (Lam PAny (Var "x")) Map.empty `shouldBe` Left (UndefinedName "x")
      check' (Lam PAny (Int 1)) Map.empty `shouldBe` Right (TFun (TVar "a") TInt)
      check' (Lam PAny (Var "a")) (Map.singleton "a" (TVar "a")) `shouldBe` Right (TFun (TVar "b") (TVar "a"))
      check' (Lam (PVar "x") (Var "x")) Map.empty `shouldBe` Right (TFun (TVar "x") (TVar "x"))
      check' (Or Tup (Int 1)) Map.empty `shouldBe` Left (TypeMismatch TTup TInt)
      check' (Or (Int 1) (Int 2)) Map.empty `shouldBe` Right TInt
      check' (App Tup (Int 1)) Map.empty `shouldBe` Left (NotAFunction Tup TTup)
      check' (App (Lam (PTup []) Tup) (Int 1)) Map.empty `shouldBe` Left (TypeMismatch TTup TInt)
      check' (App (Lam (PInt 0) Tup) (Int 1)) Map.empty `shouldBe` Right TTup
      check' (Rec []) Map.empty `shouldBe` Right (TRec [])
      check' (Rec [("x", Tup), ("y", Int 1)]) Map.empty `shouldBe` Right (TRec [("x", TTup), ("y", TInt)])

    it "☯ alternatives" $ do
      let env = defineType "Maybe" [TVar "a"] [("Just", TFun (TVar "a") (TApp (TVar "Maybe") (TVar "a"))), ("Nothing", TApp (TVar "Maybe") (TVar "a"))] Map.empty
      alternatives TInt env `shouldBe` Right [PAny]
      alternatives (TTyp []) env `shouldBe` Right []
      alternatives (TTyp ["Nothing"]) env `shouldBe` Right [PCtr "Nothing" []]
      alternatives (TVar "X") env `shouldBe` Left (UndefinedName "X")
      alternatives (TVar "Maybe") env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]
      alternatives (TApp (TVar "Maybe") (TVar "a")) env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]
      alternatives (TFun (TVar "a") (TApp (TVar "Maybe") (TVar "a"))) env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]

    it "☯ specialize" $ do
      let env = defineType "Maybe" [TVar "a"] [("Just", TFun (TVar "a") (TApp (TVar "Maybe") (TVar "a"))), ("Nothing", TApp (TVar "Maybe") (TVar "a"))] Map.empty
      specialize [] env `shouldBe` Right []
      specialize [PCtr "Nothing" []] env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]

-- it "☯ specialize" $ do
--   let env = defineType "T" [] [("A", Var "T"), ("B", Fun TInt $ Var "T")] Map.empty
--   specialize PAny env `shouldBe` Right [PAny]
--   specialize (PInt 1) env `shouldBe` Right [PAny]
--   specialize (PVar "x") env `shouldBe` Right [PAny]
--   specialize (PCtr "X" []) env `shouldBe` Left (UndefinedName "X")
--   specialize (PCtr "A" []) env `shouldBe` Right [PCtr "A" [], PCtr "B" [PAny]]
--   specialize (PCtr "B" []) env `shouldBe` Left (NumArgsMismatch {ctr = "B", expected = 1, got = 0})
--   specialize (PCtr "B" [PCtr "A" []]) env `shouldBe` Right [PCtr "A" [], PCtr "B" [PCtr "A" []], PCtr "B" [PCtr "B" [PAny]]]
--   specialize (PTup []) env `shouldBe` Right [PTup []]
--   specialize (PTup [PCtr "A" [], PCtr "A" []]) env
--     `shouldBe` Right
--       [ PTup [PCtr "A" [], PCtr "A" []],
--         PTup [PCtr "A" [], PCtr "B" [PAny]],
--         PTup [PCtr "B" [PAny], PCtr "A" []],
--         PTup [PCtr "B" [PAny], PCtr "B" [PAny]]
--       ]
