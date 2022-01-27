import ParserTests (parserTests)
import Test.Hspec
import Typed

main :: IO ()
main = hspec $ do
  parserTests

  describe "--== Typed ==--" $ do
    it "☯ defineType" $ do
      defineType "T" [] [] empty `shouldBe` fromList [("T", Typ [])]
      defineType "T" [Var "a", Var "b"] [] empty `shouldBe` fromList [("T", Fun (Var "a") $ Fun (Var "b") $ Typ [])]
      -- defineType "T" [Var "a", Var "b"] [] empty `shouldBe` fromList [("T", For "a" $ For "b" $ Fun (Var "a") $ Fun (Var "b") $ Typ [])]
      defineType "T" [] [("A", Var "T"), ("B", Fun IntT (Var "T"))] empty
        `shouldBe` fromList
          [ ("T", Typ ["A", "B"]),
            ("A", Var "T"),
            ("B", Fun IntT $ Var "T")
          ]

    it "☯ occurs" $ do
      "x" `occurs` Any `shouldBe` False
      "x" `occurs` Tup `shouldBe` False
      "x" `occurs` IntT `shouldBe` False
      "x" `occurs` Int 1 `shouldBe` False
      "x" `occurs` Var "x" `shouldBe` True
      "x" `occurs` Var "y" `shouldBe` False
      -- TODO: For
      -- TODO: Ann
      "x" `occurs` Typ ["x"] `shouldBe` False
      "x" `occurs` Fun (Var "y") (Var "y") `shouldBe` False
      "x" `occurs` Fun (Var "x") (Var "y") `shouldBe` True
      "x" `occurs` Fun (Var "y") (Var "x") `shouldBe` True
      -- TODO: Lam
      "x" `occurs` App (Var "y") (Var "y") `shouldBe` False
      "x" `occurs` App (Var "x") (Var "y") `shouldBe` True
      "x" `occurs` App (Var "y") (Var "x") `shouldBe` True

    it "☯ unify" $ do
      unify Any IntT empty `shouldBe` Right (IntT, empty)
      unify IntT Any empty `shouldBe` Right (IntT, empty)
      unify IntT Tup empty `shouldBe` Left (CannotUnify IntT Tup)
      unify IntT IntT empty `shouldBe` Right (IntT, empty)
      unify (Var "x") IntT empty `shouldBe` Right (IntT, fromList [("x", IntT)])
      unify IntT (Var "x") empty `shouldBe` Right (IntT, fromList [("x", IntT)])
      unify (Var "x") (Ann (Var "x") IntT) empty `shouldBe` Left (CannotUnify (Var "x") (Ann (Var "x") IntT))
      unify (For "x" $ Var "x") Tup empty `shouldBe` Right (Tup, fromList [("x", Tup)])
      unify Tup (For "x" $ Var "x") empty `shouldBe` Right (Tup, fromList [("x", Tup)])
      unify (Ann (Var "x") IntT) (Var "x") empty `shouldBe` Left (CannotUnify (Ann (Var "x") IntT) (Var "x"))
      unify (Ann (Var "x") IntT) (Ann (Int 1) IntT) empty `shouldBe` Right (Ann (Int 1) IntT, fromList [("x", Int 1)])
      unify (Ann (Int 1) (Var "x")) (Ann (Int 1) IntT) empty `shouldBe` Right (Ann (Int 1) IntT, fromList [("x", IntT)])
      -- unify (Ann (Int 1) (Var "x")) (Int 1) empty `shouldBe` Right (Ann (Int 1) IntT, fromList [("x", IntT)])
      unify (Fun (Var "x") IntT) (Fun Tup IntT) empty `shouldBe` Right (Fun Tup IntT, fromList [("x", Tup)])
      unify (Fun Tup (Var "x")) (Fun Tup IntT) empty `shouldBe` Right (Fun Tup IntT, fromList [("x", IntT)])

    it "☯ eval" $ do
      eval Any empty `shouldBe` Any
      eval Tup empty `shouldBe` Tup
      eval IntT empty `shouldBe` IntT
      eval (Int 1) empty `shouldBe` Int 1
      eval (Var "x") empty `shouldBe` Var "x"
      eval (Var "x") (fromList [("x", IntT)]) `shouldBe` IntT
      eval (Var "x") (fromList [("x", Ann (Var "x") IntT)]) `shouldBe` Var "x"
      eval (For "x" (Var "x")) (fromList [("x", IntT)]) `shouldBe` For "x" (Var "x")
      eval (For "x" (Var "y")) (fromList [("y", IntT)]) `shouldBe` For "x" IntT
      eval (Ann (Var "x") (Var "x")) (fromList [("x", Tup)]) `shouldBe` Tup
      eval (Typ ["A"]) empty `shouldBe` Typ ["A"]
      eval (Fun (Var "x") (Var "y")) (fromList [("x", Var "a"), ("y", Var "b")]) `shouldBe` Fun (Var "a") (Var "b")
      eval (Lam []) empty `shouldBe` Lam []
      eval (Lam [(Var "x", Int 1), (Var "x", Int 2)]) (fromList [("x", Int 0)]) `shouldBe` Lam [(Int 0, Int 1), (Int 0, Int 2)]
      eval (App (Var "x") (Int 1)) (fromList [("x", Var "x")]) `shouldBe` App (Var "x") (Int 1)
      eval (App (Lam []) (Int 0)) empty `shouldBe` Any
      eval (App (Lam [(Var "x", Var "x")]) (Int 0)) empty `shouldBe` Int 0
      eval (App (Lam [(Any, Int 1), (Any, Int 2)]) (Int 0)) empty `shouldBe` Int 1
      eval (App (Lam [(Int 1, Int 1), (Any, Int 2)]) (Int 0)) empty `shouldBe` Int 2

    it "☯ intToName" $ do
      intToName 0 `shouldBe` ""
      intToName 1 `shouldBe` "a"
      intToName 26 `shouldBe` "z"
      intToName 27 `shouldBe` "aa"
      intToName 28 `shouldBe` "ba"

    it "☯ newVar" $ do
      newVar empty `shouldBe` ("a", (fromList [("a", Var "a")]) {seed = 2})
      newVar (fromList [("a", IntT)]) `shouldBe` ("b", (fromList [("a", IntT), ("b", Var "b")]) {seed = 3})

    it "☯ rename" $ do
      let rename' x y a env = fst (rename x y a env)
      rename' "x" "y" (Var "x") empty `shouldBe` Var "y"
      rename' "x" "y" (Ann (Var "x") (Var "x")) empty `shouldBe` Ann (Var "y") (Var "y")
      rename' "x" "y" (For "x" $ Ann (Var "x") (Var "y")) empty `shouldBe` For "x" (Ann (Var "x") (Var "y"))
      rename' "x" "y" (For "y" $ Ann (Var "x") (Var "y")) empty `shouldBe` For "a" (Ann (Var "y") (Var "a"))
      rename' "x" "y" (Fun (Var "x") (Var "x")) empty `shouldBe` Fun (Var "y") (Var "y")
      rename' "x" "y" (App (Var "x") (Var "x")) empty `shouldBe` App (Var "y") (Var "y")

    it "☯ instantiate" $ do
      instantiate Any empty `shouldBe` (Any, empty)
      instantiate (For "x" (Var "x")) empty `shouldBe` (Var "x", fromList [("x", Var "x")])
      instantiate (Ann (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Ann (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
      instantiate (Fun (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Fun (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
      instantiate (Fun (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (Fun (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})
      instantiate (App (For "x" $ Var "x") (For "x" $ Var "x")) empty `shouldBe` (App (Var "x") (Var "a"), (fromList [("x", Var "x"), ("a", Var "a")]) {seed = 2})

    it "☯ typecheck" $ do
      let typecheck' a env = fmap fst (typecheck a env)
      typecheck' Any empty `shouldBe` Right Any
      typecheck' Tup empty `shouldBe` Right Tup
      typecheck' IntT empty `shouldBe` Right (Typ [])
      typecheck' (Int 1) empty `shouldBe` Right IntT
      typecheck' (Var "x") empty `shouldBe` Left (UndefinedName "x")
      typecheck' (Var "x") (fromList [("x", Int 1)]) `shouldBe` Right IntT
      typecheck' (Var "x") (fromList [("x", Var "x")]) `shouldBe` Right (Var "x")
      typecheck' (Var "x") (fromList [("x", Ann (Var "x") IntT)]) `shouldBe` Right IntT
      -- typecheck' (For "x" $ Var "x") empty `shouldBe` Right (For "x" $ Var "x")
      typecheck' (For "x" $ Int 1) empty `shouldBe` Right IntT
      typecheck' (Ann (Int 1) Tup) empty `shouldBe` Left (CannotUnify Tup IntT)
      typecheck' (Ann (Int 1) IntT) empty `shouldBe` Right IntT
      typecheck' (Typ ["A"]) empty `shouldBe` Right (Typ [])
      typecheck' (Fun (Int 1) (Int 2)) empty `shouldBe` Right (Fun IntT IntT)
      typecheck' (Lam []) empty `shouldBe` Right (Fun Any Any)
      typecheck' (Lam [(Any, Int 1)]) empty `shouldBe` Right (Fun Any IntT)
      typecheck' (Lam [(For "x" $ Var "x", Var "x")]) empty `shouldBe` Right (Fun (Var "x") (Var "x"))
      typecheck' (Lam [(Any, Tup), (Int 1, Int 2)]) empty `shouldBe` Left (CannotUnify Tup IntT)
      typecheck' (Lam [(Any, Tup), (Int 1, Any)]) empty `shouldBe` Right (Fun IntT Tup)
      typecheck' (App (Int 1) Tup) empty `shouldBe` Left (CannotUnify IntT (Fun Tup (Var "a")))
      typecheck' (App (Lam []) (Int 1)) empty `shouldBe` Right (Var "a")
      typecheck' (App (Lam [(Tup, Int 1)]) (Int 2)) empty `shouldBe` Left (CannotUnify Tup IntT)
      typecheck' (App (Lam [(Tup, Int 1)]) Tup) empty `shouldBe` Right IntT

-- it "☯ boundVars" $ do
--   let env = fromList [("x", IntT)]
--   boundVars (Var "x") env `shouldBe` Set.singleton "x"
--   boundVars (Var "y") env `shouldBe` Set.empty
--   boundVars (Ann (Var "x") (Var "x")) env `shouldBe` Set.singleton "x"
--   boundVars (Fun (Var "x") (Var "x")) env `shouldBe` Set.singleton "x"
--   boundVars (App (Var "x") (Var "x")) env `shouldBe` Set.singleton "x"

-- it "☯ rename" $ do
--   rename ["x"] empty `shouldBe` empty
--   rename ["x"] (fromList [("x", IntT)]) `shouldBe` (fromList [("x", Var "a"), ("a", Var "a")]) {seed = 2}
--   rename ["y", "x"] (fromList [("x", IntT)]) `shouldBe` (fromList [("x", Var "a"), ("a", Var "a")]) {seed = 2}
--   rename ["x", "y"] (fromList [("x", IntT), ("y", IntT)]) `shouldBe` (fromList [("x", Var "a"), ("y", Var "b"), ("a", Var "a"), ("b", Var "b")]) {seed = 3}

-- it "☯ instantiate" $ do
--   let env =
--         fromList
--           [ ("Maybe", Ann (Var "Maybe") (Fun Any $ Typ ["Just", "Nothing"])),
--             ("Just", Ann (Var "Just") (For "a" $ Fun (Var "a") $ App (Var "Maybe") (Var "a"))),
--             ("Nothing", Ann (Var "Nothing") (For "a" $ App (Var "Maybe") (Var "a")))
--           ]
--   instantiate (Var "Maybe") env `shouldBe` (App (Var "Maybe") (Var "a"), env)
--   instantiate (App (Var "Maybe") (Var "a")) empty `shouldBe` (App (Var "Maybe") (Var "a"), empty)
--   instantiate (App (Var "Maybe") (Var "a")) (fromList [("a", IntT)]) `shouldBe` (App (Var "Maybe") (Var "b"), (fromList [("a", IntT)]))

-- it "☯ alternatives" $ do
--   let env = defineType "Maybe" [Var "a"] [("Just", Fun (Var "a") (App (Ctr "Maybe") (Var "a"))), ("Nothing", App (Ctr "Maybe") (Var "a"))] empty
--   alternatives IntT env `shouldBe` Right [PAny]
--   alternatives (Typ []) env `shouldBe` Right []
--   alternatives (Typ ["Nothing"]) env `shouldBe` Right [PCtr "Nothing" []]
--   alternatives (Var "X") env `shouldBe` Left (UndefinedName "X")
--   alternatives (Var "Maybe") env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]
--   alternatives (App (Var "Maybe") (Var "a")) env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]
--   alternatives (Fun (Var "a") (App (Var "Maybe") (Var "a"))) env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]

-- it "☯ specialize" $ do
--   let env = defineType "Maybe" [Var "a"] [("Just", Fun (Var "a") (App (Ctr "Maybe") (Var "a"))), ("Nothing", App (Ctr "Maybe") (Var "a"))] empty
--   specialize [] env `shouldBe` Right []
--   specialize [PInt 1] env `shouldBe` Right [PAny]
--   specialize [PCtr "Nothing" []] env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]
--   specialize [PCtr "Just" []] env `shouldBe` Right [PCtr "Just" [PAny], PCtr "Nothing" []]

-- it "☯ specialize" $ do
--   let env = defineType "T" [] [("A", Var "T"), ("B", Fun IntT $ Var "T")] Map.empty
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
