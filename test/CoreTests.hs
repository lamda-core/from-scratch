module CoreTests where

import Core
import Test.Hspec

coreTests :: SpecWith ()
coreTests = describe "--== Core ==--" $ do
  let (i0, i1, i2, i3) = (Int 0, Int 1, Int 2, Int 3)
  let (x, y, z) = (Var "x", Var "y", Var "z")

  describe "☯ helper functions" $ do
    it "☯ lam" $ do
      lam [] Tup `shouldBe` Tup
      lam [x] Tup `shouldBe` Lam x Tup
      lam [x, y, z] Tup `shouldBe` Lam x (Lam y (Lam z Tup))
    it "☯ app" $ do
      app Tup [] `shouldBe` Tup
      app Tup [x] `shouldBe` App Tup x
      app Tup [x, y, z] `shouldBe` App (App (App Tup x) y) z
    it "☯ add" $ do
      add x y `shouldBe` App (App Add x) y
    it "☯ sub" $ do
      sub x y `shouldBe` App (App Sub x) y
    it "☯ mul" $ do
      mul x y `shouldBe` App (App Mul x) y

  describe "☯ reduction rules" $ do
    it "☯ reduce" $ do
      -- Simple cases
      reduce Any [] `shouldBe` Right (Any, [])
      reduce Tup [] `shouldBe` Right (Tup, [])
      reduce Add [] `shouldBe` Right (Add, [])
      reduce Sub [] `shouldBe` Right (Sub, [])
      reduce Mul [] `shouldBe` Right (Mul, [])
      reduce IntT [] `shouldBe` Right (IntT, [])
      reduce i1 [] `shouldBe` Right (i1, [])
      reduce (Or x y) [] `shouldBe` Right (Or x y, [])
      reduce (Ann x y) [] `shouldBe` Right (Ann x y, [])
      reduce (Lam x y) [] `shouldBe` Right (Lam x y, [])
      reduce (App Tup x) [] `shouldBe` Right (App Tup x, [])
      -- Variable lookup
      reduce x [] `shouldBe` Left (UndefinedVariable "x")
      reduce x [("x", x)] `shouldBe` Right (x, [("x", x)])
      reduce x [("x", i1), ("y", i2)] `shouldBe` Right (i1, [("x", i1), ("y", i2)])
      reduce y [("x", i1), ("y", i2)] `shouldBe` Right (i2, [("x", i1), ("y", i2)])
      -- For all
      reduce (For "x" x) [] `shouldBe` Right (x, [("x", x)])
      -- Closure reduction
      reduce (Let [("x", add i0 i1)] x) [] `shouldBe` Right (i1, [])
      -- Arithmetic reduction
      reduce (add x x) [("x", x)] `shouldBe` Right (add x x, [("x", x)])
      reduce (add x x) [("x", i1)] `shouldBe` Right (i2, [("x", i1)])
      reduce (sub x x) [("x", i1)] `shouldBe` Right (i0, [("x", i1)])
      reduce (mul x x) [("x", i1)] `shouldBe` Right (i1, [("x", i1)])
      -- Variable reduction
      reduce x [("x", add i1 i1)] `shouldBe` Right (i2, [("x", i2)])
      -- Pattern matching
      reduce (App (Lam i1 i2) (add i1 i0)) [] `shouldBe` Right (i2, [])
      reduce (App (Lam i1 i2) (add i1 i2)) [] `shouldBe` Left (PatternMismatch i1 (i3, []))
      -- Pattern matching alternatives
      reduce (App (Or (Lam i1 i2) (Lam i2 i3)) (add i1 i0)) [] `shouldBe` Right (i2, [])
      reduce (App (Or (Lam i1 i2) (Lam i2 i3)) (add i1 i1)) [] `shouldBe` Right (i3, [])
      reduce (App (Or (Lam i1 i2) (Lam i2 i3)) (add i0 i0)) [] `shouldBe` Left (PatternMismatch i2 (i0, []))
      -- Indirect reduction
      reduce (App x i1) [("x", App Add i1)] `shouldBe` Right (i2, [("x", App Add i1)])
      reduce (App x i1) [("x", App Sub i1)] `shouldBe` Right (i0, [("x", App Sub i1)])
      reduce (App x i1) [("x", App Mul i1)] `shouldBe` Right (i1, [("x", App Mul i1)])
      reduce (App x i1) [("x", Lam i1 i2)] `shouldBe` Right (i2, [("x", Lam i1 i2)])
      reduce (App x i1) [("x", Or (Lam i1 i2) (Lam i2 i3))] `shouldBe` Right (i2, [("x", Or (Lam i1 i2) (Lam i2 i3))])

    it "☯ match" $ do
      let env = [("x", x), ("y", y)]
      let env' = [("x", add i0 i1), ("y", i2), ("z", Tup)]
      -- Match anything
      match (Any, env) (Tup, env') `shouldBe` Right (env, env')
      -- Simple equality
      match (i1, env) (i1, env') `shouldBe` Right (env, env')
      match (i1, env) (i2, env') `shouldBe` Left (PatternMismatch i1 (i2, env'))
      -- Reduction
      match (i1, env) (add x x, env') `shouldBe` Left (PatternMismatch i1 (i2, [("x", i1), ("y", i2), ("z", Tup)]))
      match (i2, env) (add x x, env') `shouldBe` Right (env, [("x", i1), ("y", i2), ("z", Tup)])
      -- Variable bindings
      match (x, []) (add x x, env') `shouldBe` Left (UndefinedVariable "x")
      match (x, env) (add x x, env') `shouldBe` Right ([("x", Let env' (add x x)), ("y", y)], env')
      match (y, env) (add x x, env') `shouldBe` Right ([("x", x), ("y", Let env' (add x x))], env')
      -- Closure matching
      match (Let env' x, env) (i1, []) `shouldBe` Right (env, [])
      -- Pattern cases
      match (Or x i0, env) (add x x, env') `shouldBe` Right ([("x", Let env' (add x x)), ("y", y)], env')
      match (Or i0 x, env) (add x x, env') `shouldBe` Right ([("x", Let [("x", i1), ("y", i2), ("z", Tup)] i2), ("y", y)], [("x", i1), ("y", i2), ("z", Tup)])
      match (Or i0 i1, env) (add x x, env') `shouldBe` Left (PatternMismatch i1 (i2, [("x", i1), ("y", i2), ("z", Tup)]))
      -- Superposition
      match (i1, env) (Or x y, env') `shouldBe` Right (env, [("x", i1), ("y", i2), ("z", Tup)])
      match (i2, env) (Or x y, env') `shouldBe` Right (env, [("x", i1), ("y", i2), ("z", Tup)])
      match (i3, env) (Or x y, env') `shouldBe` Left (PatternMismatch i3 (i2, [("x", i1), ("y", i2), ("z", Tup)]))
      -- Type reflection
      match (Ann x y, env) (Ann x IntT, env') `shouldBe` Right ([("x", Let env' x), ("y", Let env' IntT)], env')
      match (Ann x y, env) (i1, env') `shouldBe` Right ([("x", Let env' i1), ("y", Let env' IntT)], env')
      -- Function matching
      match (Lam x y, env) (Lam x y, env') `shouldBe` Right ([("x", Let env' x), ("y", Let env' y)], env')
      match (Lam x x, env) (Lam x x, env') `shouldBe` Right ([("x", i1), ("y", y)], [("x", i1), ("y", i2), ("z", Tup)])
      match (Lam x x, env) (Lam x y, env') `shouldBe` Left (PatternMismatch i1 (i2, [("x", add i0 i1), ("y", i2), ("z", Tup)]))
      -- Application matching
      match (App x y, env) (App z x, env') `shouldBe` Right ([("x", Let env' z), ("y", Let env' x)], env')
      match (App x x, env) (App z z, env') `shouldBe` Right ([("x", Tup), ("y", y)], [("x", add i0 i1), ("y", i2), ("z", Tup)])
      match (App x x, env) (App z x, env') `shouldBe` Left (PatternMismatch Tup (i1, [("x", i1), ("y", i2), ("z", Tup)]))

    it "☯ eval" $ do
      let env = [("x", add i0 i1), ("y", add i1 i1), ("z", Tup)]
      eval Any env `shouldBe` Right (Any, env)
      eval Tup env `shouldBe` Right (Tup, env)
      eval Add env `shouldBe` Right (Add, env)
      eval Sub env `shouldBe` Right (Sub, env)
      eval Mul env `shouldBe` Right (Mul, env)
      eval IntT env `shouldBe` Right (IntT, env)
      eval i1 env `shouldBe` Right (i1, env)
      eval x [] `shouldBe` Left (UndefinedVariable "x")
      eval x env `shouldBe` Right (i1, [("x", i1), ("y", add i1 i1), ("z", Tup)])
      eval (For "x" y) env `shouldBe` Right (i2, [("x", x), ("x", add i0 i1), ("y", i2), ("z", Tup)])
      eval (Or x y) env `shouldBe` Right (Or i1 i2, [("x", i1), ("y", i2), ("z", Tup)])
      eval (Ann x IntT) env `shouldBe` Right (i1, [("x", i1), ("y", add i1 i1), ("z", Tup)])
      eval (Lam x y) env `shouldBe` Right (Lam i1 i2, [("x", i1), ("y", i2), ("z", Tup)])
      eval (App z x) env `shouldBe` Right (App Tup i1, [("x", i1), ("y", add i1 i1), ("z", Tup)])

      -- it "☯ typeOf" $ do
      -- Any
      -- Tup
      -- Add
      -- Sub
      -- Mul
      -- IntT
      -- Int Int
      -- Var String
      -- For String Expr
      -- Cls [(String, Expr)] Expr
      -- Or Expr Expr
      -- Ann Expr Typ
      -- Lam Pattern Expr
      -- App Expr Expr
      True `shouldBe` True

  describe "☯ parse" $ do
    it "☯ terms" $ do
      parse "_" `shouldBe` Right Any
      parse "()" `shouldBe` Right Tup
      parse "(+)" `shouldBe` Right Add
      parse "(-)" `shouldBe` Right Sub
      parse "(-)" `shouldBe` Right Sub
      parse "(*)" `shouldBe` Right Mul
      parse "%I" `shouldBe` Right IntT
      parse "1" `shouldBe` Right i1
      parse "x" `shouldBe` Right x
      parse "@x y" `shouldBe` Right (For "x" y)
      parse "@x @y z" `shouldBe` Right (For "x" (For "y" z))
      parse "x = 1; y" `shouldBe` Right (Let [("x", i1)] y)
      parse "x = 1; y = 2; z" `shouldBe` Right (Let [("x", i1), ("y", i2)] z)
      parse "(_)" `shouldBe` Right Any
      parse "( _ )" `shouldBe` Right Any

    it "☯ binary operators" $ do
      parse "1 | 2" `shouldBe` Right (Or i1 i2)
      parse "1 : 2" `shouldBe` Right (Ann i1 i2)
      parse "1 -> 2" `shouldBe` Right (Lam i1 i2)
      parse "1 + 2" `shouldBe` Right (add i1 i2)
      parse "1 - 2" `shouldBe` Right (sub i1 i2)
      parse "1 * 2" `shouldBe` Right (mul i1 i2)
      parse "1 2" `shouldBe` Right (App i1 i2)

  describe "☯ real world tests" $ do
    it "☯ factorial (simple recursion)" $ do
      -- factorial 0 = 1
      -- factorial n = n * factorial (n - 1)
      let f = Var "f"
      let n = Var "n"
      let case1 = Lam i0 i1
      let case2 = For "n" (Lam n $ mul n $ App f $ sub n i1)
      let env = [("f", case1 `Or` case2)]
      reduce (app f [Int 0]) env `shouldBe` Right (Int 1, env)
      reduce (app f [Int 1]) env `shouldBe` Right (Int 1, ("n", Int 1) : env)
      -- reduce (app f [Int 2]) env `shouldBe` Right (Int 2, ("n", i2) : ("n", i1) : env)
      True `shouldBe` True

    it "☯ ackermann (complex recursion)" $ do
      -- a 0 n = n + 1
      -- a m 0 = a (m - 1) 1
      -- a m n = a (m - 1) (a m (n - 1))
      let a = Var "a"
      let m = Var "m"
      let n = Var "n"
      let case1 = Lam i0 (For "n" $ Lam n $ add n i1)
      let case2 = For "m" (Lam i0 $ app a [sub m i1, i1])
      let case3 = For "m" (For "n" $ app a [sub m i1, app a [m, sub n i1]])
      let env = [("a", case1 `Or` case2 `Or` case3)]
      reduce (app a [Int 0]) env `shouldBe` Right (Lam n (add n i1), ("n", n) : env)
      -- reduce (app a [Int 1]) env `shouldBe` Right (Lam n (add n i1), ("n", n) : env)
      True `shouldBe` True

-- TODO: map (parallelization)
-- TODO: foldr (lazy evaluation)
-- TODO: foldl (tail call optimization)
