{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
-- import Hedgehog.Main
import GfLsp
-- import Language.LSP.Test
import Test.Tasty as Tasty
import Test.Tasty.HUnit
import Reactor
import Text.RawString.QQ
import Text.ParserCombinators.ReadP (readP_to_S)



prop_test :: Property
prop_test = property $ do
  doGfLsp === "GfLsp"

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      -- parseWarningsFromString (tail warningAndError) @?= []
      pure ()

  , testCase "Try parsing thing" $ do
     readP_to_S parseTree warningAndError @?= []
    --  readP_to_S parseForest warningAndError @?= []
    --  warningAndError @?= []
  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2 :: Int] @?= GT
  ]

-- main = defaultMain [
--   checkParallel $$(discover)
--   -- , Language.LSP.Test.anyRequest
--   ]


warningAndError :: String
warningAndError = tail [r|
PizzaEng.gf:
  Warning: function Firends is not in abstract
  Warning: category Phr is not in abstract
  Warning: no linearization of Bar
  Warning: no linearization type for Foo, inserting default {s : Str}
  Warning: no linearization type for S, inserting default {s : Str}
PizzaEng.gf:
  PizzaEng.gf:29:
    Happened in linearization of Hello
      A function type is expected for mkPhrase (happily ("hello"
                                                          ++ r)) instead of type Phrase

  ** Maybe you gave too many arguments to mkPhrase
  |]


{-
Both warning and error:

PizzaEng.gf:
   Warning: function Firends is not in abstract
   Warning: category Phr is not in abstract
   Warning: no linearization of Bar
   Warning: no linearization type for Foo, inserting default {s : Str}
   Warning: no linearization type for S, inserting default {s : Str}
PizzaEng.gf:
   PizzaEng.gf:29:
     Happened in linearization of Hello
      A function type is expected for mkPhrase (happily ("hello"
                                                           ++ r)) instead of type Phrase

   ** Maybe you gave too many arguments to mkPhrase



-}
