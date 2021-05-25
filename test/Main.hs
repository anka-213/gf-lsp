{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import GfLsp

prop_test :: Property
prop_test = property $ do
  doGfLsp === "GfLsp"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
