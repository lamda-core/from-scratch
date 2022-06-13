module CoreTests where

import Core
import Test.Hspec

coreTests :: SpecWith ()
coreTests = describe "--== Core ==--" $ do
  let (i0, i1, i2) = (Int 0, Int 1, Int 2)
  let (x, y, z) = (Var "x", Var "y", Var "z")
  let (a, b, c) = (Ctr "A", Ctr "B", Ctr "C")

  it "☯ occurs" $ do
    occurs "x" Err `shouldBe` False
    occurs "x" (Var "x") `shouldBe` True
    occurs "x" (Var "y") `shouldBe` False
    occurs "x" (Ctr "x") `shouldBe` False
    occurs "x" (Let [("x", a)] x) `shouldBe` False
    occurs "x" (Let [("y", a)] x) `shouldBe` True
    occurs "x" (Lam x x) `shouldBe` False
    occurs "x" (Lam y y) `shouldBe` False
    occurs "x" (Lam y x) `shouldBe` True
    occurs "x" (Or y y) `shouldBe` False
    occurs "x" (Or x y) `shouldBe` True
    occurs "x" (Or y x) `shouldBe` True
    occurs "x" (App y y) `shouldBe` False
    occurs "x" (App x y) `shouldBe` True
    occurs "x" (App y x) `shouldBe` True

  it "☯ constants" $ do
    reduce Err `shouldBe` Err
    reduce i0 `shouldBe` i0
    reduce a `shouldBe` a
    reduce (Lam x y) `shouldBe` Lam x y
    reduce (Let [("x", a)] Err) `shouldBe` Err
    reduce (Let [("x", a)] i0) `shouldBe` i0
    reduce (Let [("x", a)] a) `shouldBe` a
  it "☯ variables" $ do
    reduce (Let [("x", a)] x) `shouldBe` a
    reduce (Let [("x", a)] y) `shouldBe` Err
  it "☯ closures" $ do
    reduce (Let [("x", a)] $ Let [("y", a)] y) `shouldBe` a
  -- reduce (Let [("x", a)] $ Let [("y", a)] x) `shouldBe` Err
  it "☯ environment propagation" $ do
    reduce (Let [("x", a)] $ Lam x y) `shouldBe` Lam x (Let [("x", a)] y)
    reduce (Let [("x", a)] $ App x y) `shouldBe` App a (Let [("x", a)] y)
  it "☯ alternation" $ do
    reduce (Let [("x", a)] $ Or x y) `shouldBe` a
    reduce (Let [("x", a)] $ Or y x) `shouldBe` a
    reduce (Let [("x", a)] $ Or (Lam x x) y) `shouldBe` Or (Lam x (Let [("x", a)] x)) (Let [("x", a)] y)
  it "☯ application" $ do
    reduce (Let [("x", Tup)] $ App x a) `shouldBe` App Tup (Let [("x", Tup)] a)
    reduce (Let [("x", Add)] $ App x i0) `shouldBe` App Add (Let [("x", Add)] i0)
    reduce (Let [("x", a)] $ App x b) `shouldBe` App a (Let [("x", a)] b)
    reduce (Let [("x", App a b)] $ App x c) `shouldBe` App (App a (Let [("x", App a b)] b)) (Let [("x", App a b)] c)
  it "☯ error propagation" $ do
    reduce (Let [("x", Err)] $ App x b) `shouldBe` Err
  it "☯ arithmetic reductions" $ do
    reduce (Let [("x", a)] $ add x x) `shouldBe` add a a
    reduce (Let [("x", i1)] $ add x x) `shouldBe` i2
    reduce (Let [("x", i1)] $ sub x x) `shouldBe` i0
    reduce (Let [("x", i1)] $ mul x x) `shouldBe` i1
  it "☯ constant matching" $ do
    reduce (Let [("x", a)] $ App (Lam a b) x) `shouldBe` b
    reduce (Let [("x", b)] $ App (Lam a b) x) `shouldBe` Err
  it "☯ error recovery" $ do
    reduce (Let [("x", Err)] $ App (Lam Err b) x) `shouldBe` b
  it "☯ variable binding" $ do
    reduce (Let [("x", a)] $ App (Lam y y) x) `shouldBe` a
  it "☯ pattern evaluation" $ do
    reduce (Let [("x", a)] $ App (Lam (Let [("y", a)] y) b) x) `shouldBe` b
  -- TODO: pattern Lam
  it "☯ application matching" $ do
    reduce (Let [("x", App a b)] $ App (Lam (App x y) x) x) `shouldBe` a
    reduce (Let [("x", App a b)] $ App (Lam (App x y) y) x) `shouldBe` b
    reduce (Let [("x", a)] $ App (Lam (App x y) y) x) `shouldBe` Err
  it "☯ case alternation" $ do
    reduce (Let [("x", a)] $ App (Or (Lam a b) (Lam b c)) x) `shouldBe` b
    reduce (Let [("x", b)] $ App (Or (Lam a b) (Lam b c)) x) `shouldBe` c
    reduce (Let [("x", c)] $ App (Or (Lam a b) (Lam b c)) x) `shouldBe` Err
  it "☯ pattern alternation" $ do
    reduce (Let [("x", a)] $ App (Lam (Or a b) c) x) `shouldBe` c
    reduce (Let [("x", b)] $ App (Lam (Or a b) c) x) `shouldBe` c
    reduce (Let [("x", c)] $ App (Lam (Or a b) c) x) `shouldBe` Err

  it "☯ syntax sugar" $ do
    lam [] Tup `shouldBe` Tup
    lam [x] Tup `shouldBe` Lam x Tup
    lam [x, y, z] Tup `shouldBe` Lam x (Lam y (Lam z Tup))
    app Tup [] `shouldBe` Tup
    app Tup [x] `shouldBe` App Tup x
    app Tup [x, y, z] `shouldBe` App (App (App Tup x) y) z
    add x y `shouldBe` App (App Add x) y
    sub x y `shouldBe` App (App Sub x) y
    mul x y `shouldBe` App (App Mul x) y

  it "☯ factorial (simple recursion)" $ do
    -- f 0 = 1
    -- f n = n * f (n - 1)
    let f = Var "f"
    let n = Var "n"
    let case1 = Lam i0 i1
    let case2 = Lam n (mul n $ App f $ sub n i1)
    let env = [("f", Or case1 case2)]
    reduce (Let env $ app f [Int 0]) `shouldBe` Int 1

-- reduce (Let env $ app f [Int 1]) `shouldBe` Int 1
-- reduce (Let env $ app f [Int 2]) `shouldBe` Int 2
-- reduce (Let env $ app f [Int 5]) `shouldBe` Int 120

-- it "☯ ackermann (complex recursion)" $ do
--   -- a 0 n = n + 1
--   -- a m 0 = a (m - 1) 1
--   -- a m n = a (m - 1) (a m (n - 1))
--   let a = Var "a"
--   let m = Var "m"
--   let n = Var "n"
--   let case1 = Lam i0 (Lam n $ add n i1)
--   let case2 = Lam m (Lam i0 $ app a [sub m i1, i1])
--   let case3 = Lam m (Lam n $ app a [sub m i1, app a [m, sub n i1]])
--   let env = [("a", Or case1 $ Or case2 case3)]
--   reduce (Let env $ app a [i0]) `shouldBe` (Or (Lam n $ add n i1) (App (Or case2 case3) i0), env)
--   reduce (Let env $ app a [i1]) `shouldBe` (Or (Lam i0 $ app a [sub m i1, i1]) (App case3 i1), env)
--   reduce (Let env $ app a [i2]) `shouldBe` (Or (Lam i0 $ app a [sub m i1, i1]) (App case3 i2), env)
--   reduce (Let env $ app a [i0, i0]) `shouldBe` (Int 1, env)
--   reduce (Let env $ app a [i0, i1]) `shouldBe` (Int 2, env)
--   reduce (Let env $ app a [i0, i2]) `shouldBe` (Int 3, env)
--   -- reduce (Let env $ app a [i1, i0]) `shouldBe` (Int 2, env)
--   -- reduce (Let env $ app a [i1, i1]) `shouldBe` (Int 2, env)
--   True `shouldBe` True

-- TODO: map (parallelization)
-- TODO: foldr (lazy evaluation)
-- TODO: foldl (tail call optimization)
