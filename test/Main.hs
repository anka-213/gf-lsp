{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import GfLsp
-- import Language.LSP.Test
import Test.Tasty as Tasty
import Test.Tasty.HUnit
import Reactor


prop_test :: Property
prop_test = property $ do
  doGfLsp === "GfLsp"

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" []

unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT

  -- the following test does not hold
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= LT
  ]

-- main = defaultMain [
--   checkParallel $$(discover)
--   -- , Language.LSP.Test.anyRequest
--   ]
