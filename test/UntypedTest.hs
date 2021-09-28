import Test.Hspec
import Untyped

main :: IO ()
main = hspec $ do
  describe "☯︎ Number" $ do
    it "1  ∴  1" $ do
      eval (Num 1) [] `shouldBe` Right (Num 1)

  describe "☯︎ Variable" $ do
    it "x  ∴  Undefined variable: x" $ do
      eval (Var "x") [] `shouldBe` Left (UndefinedVar "x")
    it "x  Γ{x: x}  ∴  x" $ do
      eval (Var "x") [("x", Var "x")] `shouldBe` Right (Var "x")
    it "x  Γ{y: 1, x: y}  ∴  1" $ do
      eval (Var "x") [("y", Num 1), ("x", Var "y")] `shouldBe` Right (Num 1)

  -- describe "☯︎ Let bindings" $ do
  --   it "{} x  Γ{x: 1}  ∴  1" $ do
  --     eval (Let [] (Var "x")) [("x", Num 1)] `shouldBe` Right (Num 1)
  --   it "{x: 1} x  Γ{x: 2}  ∴  1" $ do
  --     eval (Let [("x", Num 1)] (Var "x")) [("x", Num 2)] `shouldBe` Right (Num 1)

  describe "☯︎ Lamda abstraction" $ do
    it "λx. y  ∴  Undefined variable: y" $ do
      eval (Lam "x" (Var "y")) [] `shouldBe` Left (UndefinedVar "y")
    it "λx. x  ∴  λx. x" $ do
      eval (Lam "x" (Var "x")) [] `shouldBe` Right (Lam "x" (Var "x"))

  describe "☯︎ Application" $ do
    it "1 2  ∴  Not a function" $ do
      eval (App (Num 1) (Num 2)) [] `shouldBe` Left (NotAFunction (Num 1))

    it "f x  ∴  Undefined variable: f" $ do
      eval (App (Var "f") (Var "x")) [] `shouldBe` Left (UndefinedVar "f")
    it "f x  Γ{f: f}  ∴  Undefined variable: x" $ do
      eval (App (Var "f") (Var "x")) [("f", Var "f")] `shouldBe` Right (App (Var "f") (Var "x"))
    it "f x  Γ{f: λy. 1, x: x}  ∴  1" $ do
      eval (App (Var "f") (Var "x")) [("f", Lam "y" (Num 1)), ("x", Var "x")] `shouldBe` Right (Num 1)
    it "f x  Γ{f: λy. λz. 1, x: 2}  ∴  λz. 1" $ do
      eval (App (Var "f") (Var "x")) [("f", Lam "y" (Lam "z" (Num 1))), ("x", Num 1)] `shouldBe` Right (Lam "z" (Num 1))

    it "(λx. 1) x  Γ{x: 2}  ∴  1" $ do
      eval (App (Lam "x" (Num 1)) (Var "x")) [("x", Num 2)] `shouldBe` Right (Num 1)

    it "((λx. x) (λy. y)) x  Γ{x: 1}  ∴  1" $ do
      eval (App (App (Lam "x" (Var "x")) (Lam "y" (Var "y"))) (Var "x")) [("x", Num 1)] `shouldBe` Right (Num 1)

  describe "☯︎ Addition" $ do
    it "(+)  ∴  (+)" $ do
      eval Add [] `shouldBe` Right Add
    it "(+) x  Γ{x: 1}  ∴  (+) 1" $ do
      eval (App Add (Var "x")) [("x", Num 1)] `shouldBe` Right (App Add (Num 1))
    it "(+) x y  Γ{x: 1, y: 2}  ∴  3" $ do
      eval (add (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Right (Num 3)

  describe "☯︎ Subtraction" $ do
    it "(-)  ∴  (-)" $ do
      eval Sub [] `shouldBe` Right Sub
    it "(-) x  Γ{x: 1}  ∴  (-) 1" $ do
      eval (App Sub (Var "x")) [("x", Num 1)] `shouldBe` Right (App Sub (Num 1))
    it "(-) x y  Γ{x: 1, y: 2}  ∴  -1" $ do
      eval (sub (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Right (Num (-1))

  describe "☯︎ Multiplication" $ do
    it "(*)  ∴  (*)" $ do
      eval Mul [] `shouldBe` Right Mul
    it "(*) x  Γ{x: 1}  ∴  (*) 1" $ do
      eval (App Mul (Var "x")) [("x", Num 1)] `shouldBe` Right (App Mul (Num 1))
    it "(*) x y  Γ{x: 1, y: 2}  ∴  2" $ do
      eval (mul (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Right (Num 2)

  describe "☯︎ Equals" $ do
    it "(==)  ∴  (==)" $ do
      eval Eq [] `shouldBe` Right Eq
    it "(==) x  Γ{x: 1}  ∴  (==) 1" $ do
      eval (App Eq (Var "x")) [("x", Num 1)] `shouldBe` Right (App Eq (Num 1))
    it "(==) x y  Γ{x: 1, y: 2}  ∴  λTrue False. False" $ do
      eval (eq (Var "x") (Var "y")) [("x", Num 1), ("y", Num 2)] `shouldBe` Right (Lam "True" (Lam "False" (Var "False")))
    it "(==) x y  Γ{x: 2, y: 2}  ∴  λTrue False. False" $ do
      eval (eq (Var "x") (Var "y")) [("x", Num 2), ("y", Num 2)] `shouldBe` Right (Lam "True" (Lam "False" (Var "True")))
    it "(==) x y  Γ{x: 3, y: 2}  ∴  λTrue False. False" $ do
      eval (eq (Var "x") (Var "y")) [("x", Num 3), ("y", Num 2)] `shouldBe` Right (Lam "True" (Lam "False" (Var "False")))

  describe "☯︎ Factorial" $ do
    it "f  Γ{f: factorial}  ∴  factorial" $ do
      eval (Var "f") [("f", factorial)] `shouldBe` Right factorial
    it "f n  Γ{f: factorial, n: n}  ∴  factorial" $ do
      eval (App (Var "f") (Var "n")) [("f", factorial), ("n", Var "n")] `shouldBe` Right (app (eq n k0) [k1, mul n (App f (sub n k1))])
    it "f n  Γ{f: factorial, n: 0}  ∴  1" $ do
      eval (App (Var "f") (Var "n")) [("f", factorial), ("n", Num 0)] `shouldBe` Right (Num 1)
    it "f n  Γ{f: factorial, n: 1}  ∴  1" $ do
      eval (App (Var "f") (Var "n")) [("f", factorial), ("n", Num 1)] `shouldBe` Right (Num 1)
    it "f n  Γ{f: factorial, n: 2}  ∴  2" $ do
      eval (App (Var "f") (Var "n")) [("f", factorial), ("n", Num 2)] `shouldBe` Right (Num 2)
    it "f n  Γ{f: factorial, n: 5}  ∴  120" $ do
      eval (App (Var "f") (Var "n")) [("f", factorial), ("n", Num 5)] `shouldBe` Right (Num 120)

  describe "☯︎ Ackermann" $ do
    -- https://en.wikipedia.org/wiki/Ackermann_function
    it "a  Γ{a: ackermann}  ∴  ackermann" $ do
      eval (Var "a") [("a", ackermann)] `shouldBe` Right ackermann
    it "a m  Γ{a: ackermann, m: m}  ∴  λn. ackermann" $ do
      eval (App (Var "a") (Var "m")) [("a", ackermann), ("m", Var "m")] `shouldBe` Right (Lam "n" (app (eq m k0) [add n k1, app (eq n k0) [app a [sub m k1, k1], app a [sub m k1, app a [m, sub n k1]]]]))
    it "a m  Γ{a: ackermann, m: 0}  ∴  λn. n + 1" $ do
      eval (App (Var "a") (Var "m")) [("a", ackermann), ("m", Num 0)] `shouldBe` Right (Lam "n" (add n k1))
    it "a m  Γ{a: ackermann, m: 1}  ∴  a 0" $ do
      eval (App (Var "a") (Var "m")) [("a", ackermann), ("m", Num 1)] `shouldBe` Right (Lam "n" (app (eq n k0) [app a [sub m k1, k1], app a [sub m k1, app a [m, sub n k1]]]))
    it "a m  Γ{a: ackermann, m: 3}  ∴  a 0" $ do
      eval (App (Var "a") (Var "m")) [("a", ackermann), ("m", Num 3)] `shouldBe` Right (Lam "n" (app (eq n k0) [app a [sub m k1, k1], app a [sub m k1, app a [m, sub n k1]]]))
    it "a m n  Γ{a: ackermann, m: 0, n: 0}  ∴  1" $ do
      eval (app (Var "a") [Var "m", Var "n"]) [("a", ackermann), ("m", Num 0), ("n", Num 0)] `shouldBe` Right (Num 1)
    it "a m n  Γ{a: ackermann, m: 1, n: 1}  ∴  3" $ do
      eval (app (Var "a") [Var "m", Var "n"]) [("a", ackermann), ("m", Num 1), ("n", Num 1)] `shouldBe` Right (Num 3)
    it "a m n  Γ{a: ackermann, m: 1, n: 3}  ∴  5" $ do
      eval (app (Var "a") [Var "m", Var "n"]) [("a", ackermann), ("m", Num 1), ("n", Num 3)] `shouldBe` Right (Num 5)
    it "a m n  Γ{a: ackermann, m: 2, n: 0}  ∴  4" $ do
      eval (app (Var "a") [Var "m", Var "n"]) [("a", ackermann), ("m", Num 2), ("n", Num 0)] `shouldBe` Right (Num 3)
  where
    a = Var "a"
    f = Var "f"
    m = Var "m"
    n = Var "n"
    k0 = Num 0
    k1 = Num 1
    -- f 0 = 1
    -- f n = n * f (n - 1)
    factorial =
      Lam
        "n"
        ( app
            (eq n k0)
            [ k1,
              mul n (App f (sub n k1))
            ]
        )
    -- a 0 n = n + 1
    -- a m 0 = a (m-1) 1
    -- a m n = a (m-1) (a m (n-1))
    ackermann =
      lam
        ["m", "n"]
        ( app
            (eq m k0)
            [ add n k1,
              app
                (eq n k0)
                [ app a [sub m k1, k1],
                  app a [sub m k1, app a [m, sub n k1]]
                ]
            ]
        )