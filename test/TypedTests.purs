module Test.TypedTests where

import Prelude

import Control.Monad.Free (Free)
import Data.List (List(..), (:))
import Dict (KV(..), dict)
import Result (Result(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Tuples (T2(..), T3(..))
import Typed (Decons(..), Error(..), Expr(..), Matcher(..), Pattern(..), allCasesOf, checkCase, checkIntSwitch, checkMatchers, deconsType, define, defineCtr, defineType, empty, typeAlts)

typedTests :: Free TestF Unit
typedTests = 
  suite "--== Typed ==--" do
    test "☯ define" do
      define "x" (Int 42) empty # Assert.equal {vars : KV "x" (Int 42) : Nil, alts : Nil}

    test "☯ defineCtr" do
      defineCtr "T" "A" (Var "T") empty # Assert.equal {vars : KV "A" (Ann (Var "A") (Var "T")) : Nil, alts : KV "T" ("A" : Nil) : Nil}
      defineCtr "T" "A" (Var "T") {vars : Nil, alts : dict [KV "T" ("B" : Nil)]} # Assert.equal {vars : KV "A" (Ann (Var "A") (Var "T")) : Nil, alts : KV "T" ("A" : "B" : Nil) : Nil}

    test "☯ defineType" do
      defineType "T" TypT Nil empty # Assert.equal {vars : KV "T" TypT : Nil, alts : Nil}
      defineType "T" (Var "a" `To` TypT) Nil empty # Assert.equal {vars : KV "T" (Var "a" `To` TypT) : Nil, alts : Nil}
      defineType "T" TypT (KV "A" (Var "T") : Nil) empty # Assert.equal {vars : KV "A" (Ann (Var "A") (Var "T")) : KV "T" TypT : Nil, alts : KV "T" ("A" : Nil) : Nil}

    -- test "☯ typeAlts" do
    --   typeAlts (Var "T") (dict []) # Assert.equal (Err $ NotATaggedUnion (Var "T"))
    --   typeAlts (Var "T") (dict [KV "T" ("A" : "B" : Nil)]) # Assert.equal (Ok $ "A" : "B" : Nil)
    --   typeAlts (App (Var "T") IntT) (dict [KV "T" ("A" : "B" : Nil)]) # Assert.equal (Ok $ "A" : "B" : Nil)

    test "☯ allCasesOf" do
      allCasesOf TypT (dict []) # Assert.equal (PAny : Nil)
      allCasesOf (Var "T") (dict []) # Assert.equal (PAny : Nil)
      allCasesOf (Var "T") (dict [KV "T" ("A" : "B" : Nil)]) # Assert.equal (PCtr "A" : PCtr "B" : Nil)

    -- TODO: test patternType
    test "☯ deconsType" do
      deconsType Nil IntT # Assert.equal IntT
      deconsType (AndL : Nil) IntT # Assert.equal (And IntT Any)
      deconsType (AndR : Nil) IntT # Assert.equal (And Any IntT)

    test "☯ checkIntSwitch" do
      let testCheck matchers default cases env = map (\(T2 t _) -> t) (checkIntSwitch matchers default cases env)
      testCheck Nil (Then 0) [] empty # Assert.equal (Err $ UndefinedCase 0)
      testCheck Nil (Then 0) [T2 (dict []) IntT] empty # Assert.equal (Ok TypT)
      testCheck (T2 1 (Then 0) : Nil) (Then 0) [T2 (dict []) IntT] empty # Assert.equal (Ok TypT)
      testCheck (T2 1 (Then 0) : T2 1 (Then 0) : Nil) (Then 0) [T2 (dict []) IntT] empty # Assert.equal (Err $ RedundantIntCases (1 : Nil))

    test "☯ checkMatchers" do
      let testCheck matchers cases env = map (\(T2 t _) -> t) (checkMatchers matchers cases env)
      testCheck Nil [] empty # Assert.equal (Ok Any)
      testCheck (Then 0 : Nil) [T2 (dict []) IntT] empty # Assert.equal (Ok TypT)
      testCheck (Then 0 : Then 1 : Nil) [T2 (dict []) Any, T2 (dict []) IntT] empty # Assert.equal (Ok TypT)

    test "☯ checkCase" do
      let testCheck matcher cases env = map (\(T2 t _) -> t) (checkCase matcher cases env)
      testCheck (Then 0) [] empty # Assert.equal (Err $ UndefinedCase 0)
      testCheck (Then 0) [T2 (dict []) IntT] empty # Assert.equal (Ok TypT)
      testCheck (Then 0) [T2 (dict []) (Var "x")] empty # Assert.equal (Err $ UndefinedName "x")
      testCheck (Then 0) [T2 (dict [KV "x" Nil]) (Var "x")] empty # Assert.equal (Ok (Var "x"))
      -- testCheck (Switch Nil Nil) [] empty # Assert.equal (Ok $ To Any Any)
      -- testCheck (Switch (AndL : Nil) Nil) [] empty # Assert.equal (Ok $ (And Any Any) `To` Any)
      -- testCheck (Switch (AndL : Nil) (T2 (PInt 0) (Then 0) : T2 PAny (Then 0) : Nil)) [T2 (dict []) IntT] empty # Assert.equal (Ok $ (And IntT Any) `To` TypT)

    -- test "☯︎ occurs" do
    --   -- "x" `occurs` Any # Assert.equal false
    --   "x" `occurs` TypT # Assert.equal false
    --   "x" `occurs` IntT # Assert.equal false
    --   -- "x" `occurs` (Ctr "x") # Assert.equal false
    --   "x" `occurs` (Var "x") # Assert.equal true
    --   "x" `occurs` (Var "y") # Assert.equal false
    --   -- "x" `occurs` (For "x" (Var "x")) # Assert.equal false
    --   -- "x" `occurs` (For "y" (Var "x")) # Assert.equal true
    --   "x" `occurs` (Var "x" `Ann` Var "y") # Assert.equal true
    --   "x" `occurs` (Var "y" `Ann` Var "x") # Assert.equal true
    --   "x" `occurs` (Var "y" `Ann` Var "y") # Assert.equal false
    --   "x" `occurs` (Var "x" `To` Var "y") # Assert.equal true
    --   "x" `occurs` (Var "y" `To` Var "x") # Assert.equal true
    --   "x" `occurs` (Var "y" `To` Var "y") # Assert.equal false
    --   -- "x" `occurs` (Var "x" `Or` Var "y") # Assert.equal true
    --   -- "x" `occurs` (Var "y" `Or` Var "x") # Assert.equal true
    --   -- "x" `occurs` (Var "y" `Or` Var "y") # Assert.equal false
    --   "x" `occurs` (Var "x" `And` Var "y") # Assert.equal true
    --   "x" `occurs` (Var "y" `And` Var "x") # Assert.equal true
    --   "x" `occurs` (Var "y" `And` Var "y") # Assert.equal false
    --   "x" `occurs` (Var "x" `App` Var "y") # Assert.equal true
    --   "x" `occurs` (Var "y" `App` Var "x") # Assert.equal true
    --   "x" `occurs` (Var "y" `App` Var "y") # Assert.equal false
    --   "x" `occurs` Add # Assert.equal false
    --   "x" `occurs` Sub # Assert.equal false
    --   "x" `occurs` Mul # Assert.equal false

    -- test "☯︎ declare" do
    --   declare Any empty # Assert.equal empty
    --   declare IntT empty # Assert.equal empty
    --   declare (Var "x") empty # Assert.equal (dict [KV "x" (Var "x")])
    --   declare (Var "x" `To` Var "y") empty # Assert.equal (dict [KV "x" (Var "x"), KV "y" (Var "y")])
    --   -- declare (Var "x" `Or` Var "y") empty # Assert.equal empty
    --   declare (Var "x" `Ann` Var "y") empty # Assert.equal (dict [KV "x" (Var "x"), KV "y" (Var "y")])
    --   declare (Var "x" `And` Var "y") empty # Assert.equal (dict [KV "x" (Var "x"), KV "y" (Var "y")])
    --   declare (Var "x" `App` Var "y") empty # Assert.equal (dict [KV "x" (Var "x"), KV "y" (Var "y")])

    -- test "☯︎ bindVar" do
    --   bindVar "x" TypT IntT # Assert.equal IntT
    --   bindVar "x" (Var "x") IntT # Assert.equal IntT
    --   bindVar "x" IntT (Var "x") # Assert.equal IntT
    --   bindVar "x" IntT (Var "x" `To`  Var "x") # Assert.equal (IntT `To`  IntT)
    --   -- bindVar "x" IntT (Var "x" `Or`  Var "x") # Assert.equal (IntT `Or`  IntT)
    --   bindVar "x" IntT (Var "x" `And` Var "x") # Assert.equal (IntT `And` IntT)
    --   bindVar "x" IntT (Var "x" `App` Var "x") # Assert.equal (IntT `App` IntT)
    --   bindVar "x" IntT (Var "x" `Ann` Var "x") # Assert.equal (IntT `Ann` IntT)

    -- test "☯︎ unify" do
    --   let testUnify a b = map (\(T2 c s) -> T2 c (s (Var "x"))) (unify a b)
    --   testUnify Any IntT # Assert.equal (Just $ T2 IntT (Var "x"))
    --   testUnify IntT Any # Assert.equal (Just $ T2 IntT (Var "x"))
    --   testUnify TypT IntT # Assert.equal Nothing
    --   testUnify IntT IntT # Assert.equal (Just $ T2 IntT (Var "x"))
    --   testUnify (Var "x") (Var "x" `To` Var "x") # Assert.equal Nothing
    --   testUnify (Var "x") IntT # Assert.equal (Just $ T2 IntT IntT)
    --   testUnify (Var "x" `To` Var "x") (Var "x") # Assert.equal Nothing
    --   testUnify IntT (Var "x") # Assert.equal (Just $ T2 IntT IntT)
    --   testUnify (Var "x" `Ann` IntT) (TypT `Ann` Var "x") # Assert.equal Nothing
    --   testUnify (Var "x" `Ann` TypT) (TypT `Ann` Var "x") # Assert.equal (Just $ T2 (TypT `Ann` TypT) TypT)
    --   testUnify (Var "x" `To`  IntT) (TypT `To`  Var "x") # Assert.equal Nothing
    --   testUnify (Var "x" `To`  IntT) (IntT `To`  Var "x") # Assert.equal (Just $ T2 (IntT `To` IntT) IntT)
    --   -- testUnify (Var "x" `Or`  IntT) (TypT `Or`  Var "x") # Assert.equal Nothing
    --   -- testUnify (Var "x" `Or`  IntT) (IntT `Or`  Var "x") # Assert.equal (Just IntT)
    --   testUnify (Var "x" `And` IntT) (TypT `And` Var "x") # Assert.equal Nothing
    --   testUnify (Var "x" `And` IntT) (IntT `And` Var "x") # Assert.equal (Just $ T2 (IntT `And` IntT) IntT)
    --   testUnify (Var "x" `App` IntT) (TypT `App` Var "x") # Assert.equal Nothing
    --   testUnify (Var "x" `App` IntT) (IntT `App` Var "x") # Assert.equal (Just $ T2 (IntT `App` IntT) IntT)

    -- test "☯︎ check" do
    --   let testCheck a env = map (\(T2 t _) -> t) (check a env)
    --   testCheck Any empty # Assert.equal (Ok Any)
    --   testCheck TypT empty # Assert.equal (Ok TypT)
    --   testCheck IntT empty # Assert.equal (Ok TypT)
    --   testCheck (Int 1) empty # Assert.equal (Ok IntT)
    --   testCheck (Var "x") empty # Assert.equal (Err $ UndefinedName "x")
    --   testCheck (Var "x") (dict [KV "x" (Int 1)]) # Assert.equal (Ok IntT)
    --   testCheck (Var "x") (dict [KV "x" (Var "x")]) # Assert.equal (Ok (Var "x"))
    --   testCheck (Var "x") (dict [KV "x" (Ann (Var "x") IntT)]) # Assert.equal (Ok IntT)
    --   -- TODO: Fix x a
    --   testCheck (Ann TypT IntT) empty # Assert.equal (Err $ TypeMismatch TypT IntT)
    --   testCheck (Ann IntT TypT) empty # Assert.equal (Ok TypT)
    --   testCheck (Ann IntT (Var "x")) empty # Assert.equal (Ok TypT)
    --   testCheck (To Any IntT) empty # Assert.equal (Ok (Any `To` TypT))
    --   testCheck (To (Var "x") (Var "x")) empty # Assert.equal (Ok (Var "x" `To` Var "x"))
    --   testCheck (To (Var "x") (Ann (Var "x") IntT)) empty # Assert.equal (Ok (IntT `To` IntT))
    --   testCheck (To (Ann (Var "x") IntT) (Var "x")) empty # Assert.equal (Ok (IntT `To` IntT))
    --   -- TODO: Lam
    --   -- testCheck (Or (Int 1) IntT) empty # Assert.equal (Err $ TypeMismatch IntT TypT)
    --   -- testCheck (Or (Int 1) (Int 2)) empty # Assert.equal (Ok $ T2 IntT (Var "x"))
    --   -- testCheck (Or IntT (Ann IntT (Var "x"))) empty # Assert.equal (Ok $ T2 TypT TypT)
    --   -- testCheck (Or (Ann IntT (Var "x")) IntT) empty # Assert.equal (Ok $ T2 TypT TypT)
    --   testCheck (And (Int 1) IntT) empty # Assert.equal (Ok (IntT `And` TypT))
    --   testCheck (And (Int 1) (Ann IntT (Var "x"))) empty # Assert.equal (Ok (IntT `And` TypT))
    --   testCheck (And (Ann IntT (Var "x")) (Int 1)) empty # Assert.equal (Ok (TypT `And` IntT))
    --   testCheck (App IntT TypT) empty # Assert.equal (Err $ NotAFunction IntT TypT)
    --   testCheck (App (Ann (Var "x") IntT `To` Var "x") TypT) empty # Assert.equal (Err $ TypeMismatch TypT IntT)
    --   testCheck (App (Var "x" `To` Var "x") (Int 0)) empty # Assert.equal (Ok IntT)
    --   testCheck Add empty # Assert.equal (Ok (Var "a" `To` (Var "a" `To` Var "a")))
    --   testCheck Sub empty # Assert.equal (Ok (Var "a" `To` (Var "a" `To` Var "a")))
    --   testCheck Mul empty # Assert.equal (Ok (Var "a" `To` (Var "a" `To` Var "a")))

    -- test "☯︎ eval" do
    --   eval Any empty # Assert.equal (Ok $ T2 Any Any)
    --   eval TypT empty # Assert.equal (Ok $ T2 TypT TypT)
    --   eval IntT empty # Assert.equal (Ok $ T2 IntT TypT)
    --   eval (Int 1) empty # Assert.equal (Ok $ T2 (Int 1) IntT)
    --   eval (Var "x") empty # Assert.equal (Err $ UndefinedName "x")
    --   eval (Var "x") (dict [KV "x" (Var "x")]) # Assert.equal (Ok $ T2 (Var "x") (Var "x"))
    --   eval (Var "x") (dict [KV "x" (Ann (Var "x") IntT)]) # Assert.equal (Ok $ T2 (Var "x") IntT)
    --   -- -- TODO: Fix x a
    --   eval (Ann TypT IntT) empty # Assert.equal (Err $ TypeMismatch TypT IntT)
    --   eval (Ann IntT TypT) empty # Assert.equal (Ok $ T2 IntT TypT)
    --   eval (Ann (Var "x") (Var "a")) (dict [KV "x" (Var "x")]) # Assert.equal (Ok $ T2 (Var "x") (Var "a"))
    --   -- eval (To (Var "x") (Var "x")) empty # Assert.equal (Ok $ T2 (Var "x" `To` Var "x") (Var "x" `To` Var "x"))
    --   -- eval (To (Var "x") (Ann (Var "x") IntT)) empty # Assert.equal (Ok $ T2 (Var "x" `To` Var "x") (IntT `To` IntT))
    --   -- eval (To (Ann (Var "x") IntT) (Var "x")) empty # Assert.equal (Ok $ T2 (Var "x" `To` Var "x") (IntT `To` IntT))
    --   -- eval (Or (Int 1) IntT) empty # Assert.equal (Err $ TypeMismatch IntT TypT)
    --   -- eval (Or (Int 1) (Int 2)) empty # Assert.equal (Ok $ T2 IntT (Var "x"))
    --   -- eval (Or IntT (Ann IntT (Var "x"))) empty # Assert.equal (Ok $ T2 TypT TypT)
    --   -- eval (Or (Ann IntT (Var "x")) IntT) empty # Assert.equal (Ok $ T2 TypT TypT)
    --   -- eval (And (Int 1) IntT) empty # Assert.equal (Ok $ T2 (IntT `And` TypT) (Var "x"))
    --   -- eval (And (Int 1) (Ann IntT (Var "x"))) empty # Assert.equal (Ok $ T2 (IntT `And` TypT) TypT)
    --   -- eval (And (Ann IntT (Var "x")) (Int 1)) empty # Assert.equal (Ok $ T2 (TypT `And` IntT) TypT)
    --   -- eval (App IntT TypT) empty # Assert.equal (Err $ NotAFunction IntT TypT)
    --   -- eval (App (Int 1 `To` Int 2) TypT) empty # Assert.equal (Err $ TypeMismatch TypT IntT)
    --   -- eval (App (Int 1 `To` Int 2) (Ann (Int 0) (Var "x"))) empty # Assert.equal (Ok $ T2 IntT IntT)
    --   -- eval (App (Var "x" `To` Var "x") (Int 0)) empty # Assert.equal (Ok $ T2 IntT IntT)
    --   -- eval Add empty # Assert.equal (Ok $ T2 (Var "a" `To` (Var "a" `To` Var "a")) (Var "x"))
    --   -- eval Sub empty # Assert.equal (Ok $ T2 (Var "a" `To` (Var "a" `To` Var "a")) (Var "x"))
    --   -- eval Mul empty # Assert.equal (Ok $ T2 (Var "a" `To` (Var "a" `To` Var "a")) (Var "x"))

    -- test "☯︎ eval" do
    --   eval TypT empty # Assert.equal (Ok $ T2 TypT TypT)
    --   eval IntT empty # Assert.equal (Ok $ T2 IntT TypT)
    --   eval (Int 1) empty # Assert.equal (Ok $ T2 (Int 1) IntT)
    --   eval (Var "x") empty # Assert.equal (Err $ UndefinedName "x")
    --   eval (Var "x") (dict [KV "x" (T2 (Var "x") IntT)]) # Assert.equal (Ok $ T2 (Var "x") IntT)
    --   -- eval (Ctr "A") empty # Assert.equal (Err $ UndefinedName "A")
    --   -- eval (Ctr "A") (dict [KV "A" IntT]) # Assert.equal (Ok $ T2 (Ctr "A") IntT)
    --   -- TODO: Fix x a
    --   -- eval (For "x" IntT) empty # Assert.equal (Ok $ T2 IntT TypT)
    --   -- eval (For "x" (Var "x")) empty # Assert.equal (Ok $ T2 (For "x" (Var "x")) (For "x" (Var "x")))
    --   eval (Ann TypT IntT) empty # Assert.equal (Err $ TypeMismatch TypT IntT)
    --   eval (Ann IntT TypT) empty # Assert.equal (Ok $ T2 IntT TypT)
    --   eval (Int 1 `To` Int 2) empty # Assert.equal (Ok $ T2 (Int 1 `To` Int 2) (IntT `To` IntT))
    --   eval (Int 1 `Or` Int 2) empty # Assert.equal (Ok $ T2 (Int 1 `Or` Int 2) IntT)
    --   eval (Int 1 `And` Int 2) empty # Assert.equal (Ok $ T2 (Int 1 `And` Int 2) (IntT `And` IntT))
    --   eval ((Int 1 `To` Int 2) `App` TypT) empty # Assert.equal (Err $ TypeMismatch IntT TypT)
    --   eval ((Int 1 `To` Int 2) `App` Int 0) empty # Assert.equal (Err $ PatternMismatch (Int 1) (Int 0))
    --   eval (Int 1 `App` Int 2) empty # Assert.equal (Err $ NotAFunction (Int 1))
    --   eval (Var "x" `App` Int 1) (dict [KV "x" (T2 (Var "x") (IntT `To` TypT))]) # Assert.equal (Ok $ T2 (Var "x" `App` Int 1) TypT)
    --   eval ((Var "x" `To` Var "x") `App` Int 1) empty # Assert.equal (Ok $ T2 (Int 1) IntT)
    --   eval (((Int 1 `To` Int 2) `Or` (Var "x" `To` Var "x")) `App` Int 1) empty # Assert.equal (Ok $ T2 (Int 2) IntT)
    --   eval (((Int 1 `To` Int 2) `Or` (Var "x" `To` Var "x")) `App` Int 0) empty # Assert.equal (Ok $ T2 (Int 0) IntT)
    --   eval (((Int 1 `To` Int 2) `And` (Var "x" `To` Var "x")) `App` Int 1) empty # Assert.equal (Ok $ T2 (Int 2 `And` Int 1) (IntT `And` IntT))
    --   eval (((Int 1 `To` Int 2) `And` (Var "x" `To` Var "x")) `App` Int 0) empty # Assert.equal (Err $ PatternMismatch (Int 1) (Int 0))
    --   eval ((Var "x" `To` Var "x") `App` (Var "y" `To` Var "y") `App` Int 1) empty # Assert.equal (Ok $ T2 (Int 1) IntT)
    --   eval (Var "n" `To` (Var "n" `add2` Int 1)) empty # Assert.equal (Ok $ T2 (Var "n" `To` (Var "n" `add2` Int 1)) (IntT `To` IntT))
    --   eval Add empty # Assert.equal (Ok $ T2 Add (Var "a" `To` (Var "a" `To` Var "a")))
    --   eval Sub empty # Assert.equal (Ok $ T2 Sub (Var "a" `To` (Var "a" `To` Var "a")))
    --   eval Mul empty # Assert.equal (Ok $ T2 Mul (Var "a" `To` (Var "a" `To` Var "a")))
    --   eval (add2 (Int 1) (Int 2)) empty # Assert.equal (Ok $ T2 (Int 3) IntT)
    --   eval (sub2 (Int 1) (Int 2)) empty # Assert.equal (Ok $ T2 (Int (-1)) IntT)
    --   eval (mul2 (Int 1) (Int 2)) empty # Assert.equal (Ok $ T2 (Int 2) IntT)

    -- test "☯︎ recursion" do
    --   let k0 = Int 0
    --   let k1 = Int 1
    --   let a  = Var "a"
    --   let f  = Var "f"
    --   let n  = Var "n"
    --   let m  = Var "m"

    --   -- f 0 = 1
    --   -- f n = n * f (n - 1)
    --   let factorial =
    --         (k0 `To` k1)
    --         `Or` (n `To` (n `mul2` (f `App` (n `sub2` k1))))

    --   eval (Var "f") (dict [KV "f" $ T2 factorial (IntT `To` IntT)]) # Assert.equal (Ok $ T2 factorial (IntT `To` IntT))
    --   -- eval (Var "f" `App` Var "n") (dict [KV "f" $ T2 factorial (IntT `To` IntT), KV "n" (T2 (Var "n") IntT)]) # Assert.equal (Ok $ T2 (App (Var "f") (Var "n")) IntT)
    --   eval (Var "f" `App` Int 0) (dict [KV "f" $ T2 factorial (IntT `To` IntT)]) # Assert.equal (Ok $ T2 (Int 1) IntT)
    --   eval (Var "f" `App` Int 1) (dict [KV "f" $ T2 factorial (IntT `To` IntT)]) # Assert.equal (Ok $ T2 (Int 1) IntT)
    --   eval (Var "f" `App` Int 5) (dict [KV "f" $ T2 factorial (IntT `To` IntT)]) # Assert.equal (Ok $ T2 (Int 120) IntT)

    --   -- a 0 n = n + 1
    --   -- a m 0 = a (m-1) 1
    --   -- a m n = a (m-1) (a m (n-1))
    --   let ackermann =
    --         (k0 `To` (n `To` (n `add2` k1)))
    --         `Or` (
    --           m `To` (
    --             (k0 `To` (app2 a (m `sub2` k1) k1))
    --             `Or` (n `To` (app2 a (m `sub2` k1) (app2 a m (n `sub2` k1))))
    --           )
    --         )

    --   eval (Var "a") (dict [KV "a" $ T2 ackermann (IntT `To` (IntT `To` IntT))]) # Assert.equal (Ok $ T2 ackermann (IntT `To` (IntT `To` IntT)))
    --   eval (Var "a" `App` Int 0) (dict [KV "a" $ T2 ackermann (IntT `To` (IntT `To` IntT))]) # Assert.equal (Ok $ T2 (n `To` (n `add2` k1)) (IntT `To` IntT))

    -- test "☯︎ substitute" do
    --   substitute "x" (Int 1) Any # Assert.equal Any
    --   substitute "x" (Int 1) Typ # Assert.equal Typ
    --   substitute "x" (Int 1) IntT # Assert.equal IntT
    --   substitute "x" (Int 1) (Int 2) # Assert.equal (Int 2)
    --   substitute "x" (Int 1) (Ctr "x") # Assert.equal (Ctr "x")
    --   substitute "x" (Int 1) (Var "x") # Assert.equal (Int 1)
    --   substitute "x" (Int 1) (Var "y") # Assert.equal (Var "y")
    --   -- TODO: Fix
    --   substitute "x" (Int 1) (For "x" (Var "x")) # Assert.equal (For "x" (Var "x"))
    --   substitute "x" (Int 1) (For "y" (Var "x")) # Assert.equal (For "y" (Int 1))
    --   substitute "x" (Int 1) (Var "x" `To` Var "x") # Assert.equal (Int 1 `To` Int 1)
    --   substitute "x" (Int 1) (Var "x" `Or` Var "x") # Assert.equal (Int 1 `Or` Int 1)
    --   substitute "x" (Int 1) (Var "x" `And` Var "x") # Assert.equal (Int 1 `And` Int 1)
    --   substitute "x" (Int 1) (Var "x" `App` Var "x") # Assert.equal (Int 1 `App` Int 1)
    --   substitute "x" (Int 1) (Var "x" `Ann` Var "x") # Assert.equal (Int 1 `Ann` Int 1)
    --   substitute "x" (Int 1) Add # Assert.equal Add
    --   substitute "x" (Int 1) Sub # Assert.equal Sub
    --   substitute "x" (Int 1) Mul # Assert.equal Mul

    -- suite "☯︎ unify" do
    --   test "❌ Int == Type  ∴  Pattern mismatch: Int ≠ Type" do
    --     unify IntT Typ empty # Assert.equal (Err $ PatternMismatch IntT Typ)
    --   test "✅ Int == Int  ∴  Int" do
    --     unify IntT IntT empty # Assert.equal (Ok $ IntT `KV` empty)
    --   test "❌ x == _  ∴  Undefined name: x" do
    --     unify (Var "x") Any empty # Assert.equal (Err $ UndefinedName "x")
    --   test "❌ _ == y  ∴  Undefined name: y" do
    --     unify Any (Var "y") empty # Assert.equal (Err $ UndefinedName "y")
    --   test "✅ _ == Int  ∴  Int" do
    --     unify Any IntT empty # Assert.equal (Ok $ IntT `KV` empty)
    --   test "✅ Int == _  ∴  Int" do
    --     unify IntT Any empty # Assert.equal (Ok $ IntT `KV` empty)
    --   test "✅ ∀x. x == ∀y. y  ∴  ∀y. y" do
    --     unify (For "x" $ Var "x") (For "y" $ Var "y") empty # Assert.equal (Ok $ For "y" (Var "y") `KV` dict ["x" `KV` For "y" (Var "y")])
    --   test "✅ ∀x. x == Int  ∴  Int" do
    --     unify (For "x" $ Var "x") IntT empty # Assert.equal (Ok $ IntT `KV` dict ["x" `KV` IntT])
    --   test "✅ Int == ∀x. x  ∴  Int" do
    --     unify IntT (For "x" $ Var "x") empty # Assert.equal (Ok $ IntT `KV` dict ["x" `KV` IntT])
    --   test "❌ ∀x. x -> x == Int -> Type  ∴  Pattern mismatch: Int ≠ Type" do
    --     unify (For "x" $ Var "x" `To` Var "x") (IntT `To` Typ) empty # Assert.equal (Err $ PatternMismatch IntT Typ)
    --   test "✅ ∀x. x -> x == ∀x. x -> Int  ∴  Int -> Int" do
    --     unify (For "x" $ Var "x" `To` Var "x") (For "x" $ Var "x" `To` IntT) empty # Assert.equal (Ok $ (IntT `To` IntT) `KV` dict ["x'" `KV` IntT, "x" `KV` Var "x'"])
    --   test "✅ ∀x. x -> x == ∀y. y -> y  ∴  ∀y. y -> y" do
    --     unify (For "x" $ Var "x" `To` Var "x") (For "y" $ Var "y" `To` Var "y") empty # Assert.equal (Ok $ For "y" (Var "y" `To` Var "y") `KV` dict ["y" `KV` Var "y", "x" `KV` Var "y"])
    --   test "✅ Type | Int == Type  ∴  Type" do
    --     unify (Typ `Or` IntT) Typ empty # Assert.equal (Ok $ Typ `KV` empty)
    --   test "✅ Type | Int == Int  ∴  Int" do
    --     unify (Typ `Or` IntT) IntT empty # Assert.equal (Ok $ IntT `KV` empty)
    --   test "✅ Type == Type | Int  ∴  Type" do
    --     unify Typ (Typ `Or` IntT) empty # Assert.equal (Ok $ Typ `KV` empty)
    --   test "✅ Int == Type | Int  ∴  Int" do
    --     unify IntT (Typ `Or` IntT) empty # Assert.equal (Ok $ IntT `KV` empty)
    --   test "❌ ∀x. (x, x) == (Int, Type)  ∴  Pattern mismatch: Int ≠ Type" do
    --     unify (For "x" $ Var "x" `And` Var "x") (IntT `And` Typ) empty # Assert.equal (Err $ PatternMismatch IntT Typ)
    --   test "✅ ∀x. (x, x) == ∀x. (x, Int)  ∴  (Int, Int)" do
    --     unify (For "x" $ Var "x" `And` Var "x") (For "x" $ Var "x" `And` IntT) empty # Assert.equal (Ok $ (IntT `And` IntT) `KV` dict ["x'" `KV` IntT, "x" `KV` Var "x'"])
    --   test "✅ ∀x. A x == A Int  ∴  A Int" do
    --     unify (For "x" $ Ctr "A" `App` Var "x") (Ctr "A" `App` IntT) empty # Assert.equal (Ok $ (Ctr "A" `App` IntT) `KV` dict ["x" `KV` IntT])

    -- suite "☯︎ eval" do
    --   test "✅ _  ∴  _ : _" do
    --     eval Any empty # Assert.equal (Ok $ Any `KV` Any)
    --   test "✅ Type ∴  Type : Type" do
    --     eval Typ empty # Assert.equal (Ok $ Typ `KV` Typ)
    --   test "✅ Int  ∴  Int : Type" do
    --     eval IntT empty # Assert.equal (Ok $ IntT `KV` Typ)
    --   test "✅ 42  ∴  42 : Int" do
    --     eval (Int 42) empty # Assert.equal (Ok $ Int 42 `KV` IntT)

    --   test "❌ x  ∴  Undefined name: x" do
    --     eval (Var "x") empty # Assert.equal (Err $ UndefinedName "x")
    --   test "✅ x  Γ{x: x}  ∴  x : ∀x. x" do
    --     eval (Var "x") (dict ["x" `KV` Var "x"]) # Assert.equal (Ok $ Var "x" `KV` Var "x")
    --   test "✅ x  Γ{x: x : Int}  ∴  x : Int" do
    --     eval (Var "x") (dict ["x" `KV` (Var "x" `Ann` IntT)]) # Assert.equal (Ok $ Var "x" `KV` IntT)

    --   test "❌ A  ∴  Undefined name: A" do
    --     eval (Ctr "A") empty # Assert.equal (Err $ UndefinedName "A")
    --   test "✅ A  Γ{A: A : Type}  ∴  A : Type" do
    --     eval (Ctr "A") (dict ["A" `KV` (Ctr "A" `Ann` Typ)]) # Assert.equal (Ok $ Ctr "A" `KV` Typ)

    --   test "❌ ∀x. y  ∴  Undefined name: y" do
    --     eval (For "x" (Var "y")) empty # Assert.equal (Err $ UndefinedName "y")
    --   test "✅ ∀x. x  ∴  ∀x. x : ∀x. x" do
    --     eval (For "x" (Var "x")) empty # Assert.equal (Ok $ (For "x" (Var "x")) `KV` (For "x" (Var "x")))
    --   test "✅ ∀x. Int  ∴  Int : Type" do
    --     eval (For "x" IntT) empty # Assert.equal (Ok $ IntT `KV` Typ)

    --   test "❌ 1 : Type  ∴  Pattern mismatch: Int ≠ Type" do
    --     eval (Int 1 `Ann` Typ) empty # Assert.equal (Err $ PatternMismatch IntT Typ)
    --   test "✅ 1 : _  ∴  1 : Int" do
    --     eval (Int 1 `Ann` Any) empty # Assert.equal (Ok $ Int 1 `KV` IntT)

    --   test "✅ 1 -> Int  ∴  1 -> Int : Int -> Type" do
    --     eval (Int 1 `To` IntT) empty # Assert.equal (Ok $ (Int 1 `To` IntT) `KV` (IntT `To` Typ))

    --   test "❌ 1 | Int  ∴  Pattern mismatch: Int ≠ Type" do
    --     eval (Int 1 `Or` IntT) empty # Assert.equal (Err $ PatternMismatch IntT Typ)
    --   test "✅ 1 | 2  ∴  1 | 2 : Int" do
    --     eval (Int 1 `To` IntT) empty # Assert.equal (Ok $ (Int 1 `To` IntT) `KV` (IntT `To` Typ))
    --   -- TODO: test for missing cases
    --   -- TODO: test for redundant cases

    --   test "✅ (1, Type)  ∴  (1, Type) : (Int, Type)" do
    --     eval (Int 1 `And` Typ) empty # Assert.equal (Ok $ (Int 1 `And` Typ) `KV` (IntT `And` Typ))

    --   test "❌ 1 Type  ∴  Pattern mismatch: Int ≠ Type -> _" do
    --     eval (Int 1 `App` Typ) empty # Assert.equal (Err $ PatternMismatch IntT (Typ `To` Any))
    --   test "❌ (_ -> 1) x  ∴  Undefined name: x" do
    --     eval ((Any `To` Int 1) `App` Var "x") empty # Assert.equal (Err $ UndefinedName "x")
    --   test "✅ (1 -> _) 2  ∴  Pattern mismatch: 1 ≠ 2" do
    --     eval ((Int 1 `To` Any) `App` Int 2) empty # Assert.equal (Err $ PatternMismatch (Int 1) (Int 2))
    --   test "✅ (∀x. x -> x) 1  ∴  1 : Int" do
    --     eval ((For "x" $ Var "x" `To` Var "x") `App` Int 1) empty # Assert.equal (Ok $ Int 1 `KV` IntT)
    --   test "✅ (1 -> 2 | ∀x. x -> x) 1  ∴  2 : Int" do
    --     eval (((Int 1 `To` Int 2) `Or` (For "x" $ Var "x" `To` Var "x")) `App` Int 1) empty # Assert.equal (Ok $ Int 2 `KV` IntT)
    --   test "✅ (1 -> 2 | ∀x. x -> x) 3  ∴  3 : Int" do
    --     eval (((Int 1 `To` Int 2) `Or` (For "x" $ Var "x" `To` Var "x")) `App` Int 3) empty # Assert.equal (Ok $ Int 3 `KV` IntT)
    --   test "✅ (1 -> 2, ∀x. x -> x) 1  ∴  (2, 1) : (Int, Int)" do
    --     eval (((Int 1 `To` Int 2) `And` (For "x" $ Var "x" `To` Var "x")) `App` Int 1) empty # Assert.equal (Ok $ (Int 2 `And` Int 1) `KV` (IntT `And` IntT))
    --   test "✅ (1 -> 2 -> 3) 1  ∴  2 -> 3 : Int -> Int" do
    --     eval ((Int 1 `To` (Int 2 `To` Int 3)) `App` Int 1) empty # Assert.equal (Ok $ (Int 2 `To` Int 3) `KV` (IntT `To` IntT))
    --   test "✅ (1 -> 2 -> 3) 1 2  ∴  3 : Int" do
    --     eval ((Int 1 `To` (Int 2 `To` Int 3)) `App` Int 1 `App` Int 2) empty # Assert.equal (Ok $ Int 3 `KV` IntT)

    --   test "✅ (+)  ∴  (+) : ∀a. a -> a -> a" do
    --     eval Add empty # Assert.equal (Ok $ Add `KV` (For "a" $ Var "a" `To` (Var "a" `To` Var "a")))
    --   test "✅ (+) 1 2  ∴  3 : Int" do
    --     eval (App (App Add (Int 1)) (Int 2)) empty # Assert.equal (Ok $ Int 3 `KV` IntT)

    --   test "✅ (-)  ∴  (-) : ∀a. a -> a -> a" do
    --     eval Sub empty # Assert.equal (Ok $ Sub `KV` (For "a" $ Var "a" `To` (Var "a" `To` Var "a")))
    --   test "✅ (-) 2 1  ∴  1 : Int" do
    --     eval (App (App Sub (Int 2)) (Int 1)) empty # Assert.equal (Ok $ Int 1 `KV` IntT)

    --   test "✅ (*)  ∴  (*) : ∀a. a -> a -> a" do
    --     eval Mul empty # Assert.equal (Ok $ Mul `KV` (For "a" $ Var "a" `To` (Var "a" `To` Var "a")))
    --   test "✅ (*) 2 3  ∴  6 : Int" do
    --     eval (App (App Mul (Int 2)) (Int 3)) empty # Assert.equal (Ok $ Int 6 `KV` IntT)

    -- suite "☯︎ Factorial" do
    --   -- test "✅ f  Γ{f: factorial}  ∴  factorial" do
    --   --   -- eval (Var "f") (dict [KV "f" factorial]) # Assert.equal (Ok $ (factorial `As` "f") `KV` (IntT `To` IntT))
    --   --   eval (Var "f") (dict [KV "f" factorial]) # Assert.equal (Ok $ Var "f" `KV` (IntT `To` IntT))
    --   -- test "✅ f n  Γ{f: factorial, n: n}  ∴  f n : Int" do
    --   --   eval (Var "f" `App` Var "n") (dict [KV "f" factorial, "n" `KV` Var "n"]) # Assert.equal (Ok $ (Var "f" `App` Var "n") `KV` IntT)
    --   test "✅ f 0  Γ{f: factorial}  ∴  1 : Int" do
    --     eval (Var "f" `App` Int 0) (dict [KV "f" factorial]) # Assert.equal (Ok $ Int 1 `KV` IntT)
    -- --   test "✅ f 1  Γ{f: factorial}  ∴  1 : Int" do
    -- --     eval (Var "f" `App` Int 1) (dict [KV "f" factorial]) # Assert.equal (Ok $ Int 1 `KV` IntT)
    -- --   test "✅ f 2  Γ{f: factorial}  ∴  2 : Int" do
    -- --     eval (Var "f" `App` Int 2) (dict [KV "f" factorial]) # Assert.equal (Ok $ Int 2 `KV` IntT)
    -- --   test "✅ f 5  Γ{f: factorial}  ∴  120 : Int" do
    -- --     eval (Var "f" `App` Int 5) (dict [KV "f" factorial]) # Assert.equal (Ok $ Int 120 `KV` IntT)

    -- suite "☯︎ Ackermann" do
    -- --   test "✅ a  Γ{a: ackermann}  ∴  ackermann @ a" do
    -- --     eval (Var "a") (dict [KV "a" ackermann]) # Assert.equal (Ok $ Var "a" `KV` (IntT `To` (IntT `To` IntT)))
    --   test "✅ a 0  Γ{a: ackermann}  ∴  n -> n + 1" do
    --     -- TODO: fix this type!
    --     eval (App (Var "a") (Int 0)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ (For "n" $ Var "n" `To` (Var "n" `add2` Int 1)) `KV` (For "n" $ Var "n" `To` IntT))
    --   -- test "✅ a 1  Γ{a: ackermann}  ∴  n -> n + 1" do
    --   --   -- TODO: fix this type!
    --   --   eval (App (Var "a") (Int 1)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ (For "n" $ Var "n" `To` (Var "n" `add2` Int 1)) `KV` (For "n" $ Var "n" `To` IntT))
    -- --   test "✅ a 0 0  Γ{a: ackermann}  ∴  1" do
    -- --     eval (app2 (Var "a") (Int 0) (Int 0)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 1 `KV` IntT)
    -- --   test "✅ a 0 1  Γ{a: ackermann}  ∴  2" do
    -- --     eval (app2 (Var "a") (Int 0) (Int 1)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 2 `KV` IntT)
    --   -- test "✅ a 1 0  Γ{a: ackermann}  ∴  2" do
    --   --   eval (app2 (Var "a") (Int 1) (Int 0)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 2 `KV` IntT)
    -- --   -- test "✅ a 1 1  Γ{a: ackermann}  ∴  3" do
    -- --   --   eval (app2 (Var "a") (Int 1) (Int 1)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 3 `KV` IntT)
    -- --   -- test "✅ a 2 2  Γ{a: ackermann}  ∴  7" do
    -- --   --   eval (app2 (Var "a") (Int 2) (Int 2)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 7 `KV` IntT)
    -- --   -- test "✅ a 3 2  Γ{a: ackermann}  ∴  29" do
    -- --   --   eval (app2 (Var "a") (Int 3) (Int 2)) (dict [KV "a" ackermann]) # Assert.equal (Ok $ Int 29 `KV` IntT)

    -- where
    --   -- f 0 = 1
    --   -- f n = n * f (n - 1)
    --   factorial =
    --     ( Int 0 `To` Int 1
    --     ) `Or` (For "n" $ Var "n" `To`
    --         (Var "n" `mul2` (Var "f" `App` (Var "n" `sub2` Int 1)))
    --     )

    --   -- a 0 n = n + 1
    --   -- a m 0 = a (m-1) 1
    --   -- a m n = a (m-1) (a m (n-1))
    --   -- a = 0 -> n -> n + 1
    --   --   | m -> 0 -> a (m-1) 1
    --   --   | m -> n -> a (m-1) (a m (n-1))
    --   -- a = 0 -> (n -> n + 1)
    --   --   | m ->
    --   --      ( 0 -> a (m-1) 1
    --   --      | n -> a (m-1) (a m (n-1))
    --   --      )
    --   ackermann =
    --     ( Int 0 `To` (For "n" $ Var "n" `To`
    --         (Var "n" `add2` Int 1))
    --     ) `Or` (For "m" $ Var "m" `To`
    --       ((
    --         Int 0 `To` (app2 (Var "a") (Var "m" `sub2` Int 1) (Int 1))
    --       ) `Or` (
    --         For "n" $ Var "n" `To` (app2 (Var "a") (Var "m" `sub2` Int 1) (app2 (Var "a") (Var "m") (Var "n" `sub2` Int 1)))
    --       ))
    --     )
