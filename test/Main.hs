{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Main where

import           Hedgehog
-- import Hedgehog.Main
import           GfLsp
-- import Language.LSP.Test
import           Reactor
import           Test.Tasty                   as Tasty
import           Test.Tasty.HUnit
import           Text.ParserCombinators.ReadP (readP_to_S)
import           Text.RawString.QQ
import Data.Bifunctor (first)
import qualified Language.LSP.Types as J



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
     readP_to_S parseTree warningAndError @?= [(firstTree, restStr)]
  , testCase "Try parsing thing2" $ do
     readP_to_S parseForestFinal warningAndError @?= [([firstTree, secondTree, nullTree, thirdTree],"")]
    --  warningAndError @?= []
  , testCase "Try parsing warnings" $ do
      parseWarningsFromString warningAndError @?= warningsExpected
  , testCase "Inverse of tree parse" $ do -- TODO: Property test
     case readP_to_S parseForestFinal warningAndError  of
       [(inner,"")] -> forestToString inner @?= warningAndError
       e -> error $ "Unexpected: " ++ show e
  -- the following test does not hold
  -- , testCase "List comparison (same length)" $
  --     [1, 2, 3] `compare` [1,2,2 :: Int] @?= GT
  , testCase "SplitErrors" $ do
      splitErrors warningAndError @?= splitErrorsExpected
  , testCase "Example1 from source code" $ do
      oldErrorParser testCase1 @?= ex1Expected
  , testCase "Example2 from source code" $ do
      oldErrorParser testCase2 @?= ex2Expected
  , testCase "Example3 from source code" $ do
      oldErrorParser testCase3 @?= ex3Expected
  , testCase "ErrorParser warning" $ do
      oldErrorParser warningAndError @?= warnErrExpected
  ]

splitErrorsExpected :: [String]
splitErrorsExpected =
  ["PizzaEng.gf:\n  PizzaEng.gf:29:\n    Happened in linearization of Hello\n      A function type is expected for mkPhrase (happily (\"hello\"\n                                                          ++ r)) instead of type Phrase\n"
  ++ "\n  ** Maybe you gave too many arguments to mkPhrase\n"]

ex1Expected :: ([Maybe FilePath], [(J.Range, String)])
ex1Expected = ([Just "src/swedish/MorphoSwe.gf",Just "src/swedish/MorphoSwe.gf"],
  [(mkRange 31 1 41 1,"src/swedish/MorphoSwe.gf:31-40:\n  Happened in the renaming of ptPretForms\n   constant not found: funnenx\n   given Predef, Predef, Prelude, DiffSwe, ResSwe, ParamX,\n         CommonScand, MorphoSwe\n")
  ,(mkRange 20 1 30 1,"src/swedish/MorphoSwe.gf:20-29:\n  Happened in the renaming of ptPretAll\n   constant not found: kox\n   given Predef, Predef, Prelude, DiffSwe, ResSwe, ParamX,\n         CommonScand, MorphoSwe\n")
  ])

ex2Expected :: ([Maybe FilePath], [(J.Range, String)])
ex2Expected = ([Just "grammars/QuestionsEng.gf"],
  [(mkRange 35 1 36 1,"grammars/QuestionsEng.gf:\n   grammars/QuestionsEng.gf:35:\n     Happened in linearization of MkPred1\n      unknown label cxn in\n        {atype : AType;\n         cn : {s : Number => Case => Str; g : Gender; lock_CN : {}};\n         n2 : {s : Number => Case => Str; c2 : Str; g : Gender;\n               lock_N2 : {}};\n         v : {s : Order => Agr => {fin : Str; inf : Str}; lock_VPS : {}};\n         v2 : {s : Order => Agr => {fin : Str; inf : Str}; c2 : Str;\n               lock_VPS2 : {}}}\n")
  ])

-- TODO: Guess range
ex3Expected :: ([Maybe FilePath], [(J.Range, String)])
ex3Expected = ([Nothing],
  [(defRange ,"ParadigmsYrl.gf:\n   ParadigmsYrl.gf:\n     Happened in overloading mkA\n      missing record fields: s, c, v type of ss s\n      expected: {s : ResYrl.PsorForm => Str; c : ResYrl.VClass;\n                 lock_A : {}; v : ResYrl.Verbal}\n      inferred: {s : Str}\n      \n")])

warnErrExpected :: ([Maybe String], [(J.Range, String)])
warnErrExpected = ([Just "PizzaEng.gf"],
  [ (mkRange 29 1 30 1,"PizzaEng.gf:\n  PizzaEng.gf:29:\n    Happened in linearization of Hello\n      A function type is expected for mkPhrase (happily (\"hello\"\n                                                          ++ r)) instead of type Phrase\n"
     ++ "\n  ** Maybe you gave too many arguments to mkPhrase\n")])

oldErrorParser :: String -> ([Maybe FilePath], [(J.Range, String)])
oldErrorParser msg = (relFiles,diags)
  where
    msgs = splitErrors msg
    range = maybe (Nothing, defRange) (first Just) . parseErrorMessage
    (relFiles, ranges) = unzip $ map range msgs
    diags = zip ranges msgs


-- main = defaultMain [
--   checkParallel $$(discover)
--   -- , Language.LSP.Test.anyRequest
--   ]


warningsExpected :: [(String, Maybe a, (String, Maybe String))]
warningsExpected =
    [ ( "PizzaEng.gf" , Nothing , ( "Warning: function Firends is not in abstract" , Just "Firends"))
    , ( "PizzaEng.gf" , Nothing , ( "Warning: category Phr is not in abstract" , Just "Phr"))
    , ( "PizzaEng.gf" , Nothing , ( "Warning: no linearization of Bar" , Just "lin"))
    , ( "PizzaEng.gf" , Nothing , ( "Warning: no linearization type for Foo, inserting default {s : Str}" , Just "lincat"))
    , ( "PizzaEng.gf" , Nothing , ( "Warning: no linearization type for S, inserting default {s : Str}" , Just "lincat"))
    ]

firstTree :: Tree (Int, String)
firstTree = Node (0,"PizzaEng.gf:")
  [Node (2,"Warning: function Firends is not in abstract") []
  ,Node (2,"Warning: category Phr is not in abstract") []
  ,Node (2,"Warning: no linearization of Bar") []
  ,Node (2,"Warning: no linearization type for Foo, inserting default {s : Str}") []
  ,Node (2,"Warning: no linearization type for S, inserting default {s : Str}") []]

restStr :: String
restStr = "PizzaEng.gf:\n  PizzaEng.gf:29:\n    Happened in linearization of Hello\n      A function type is expected for mkPhrase (happily (\"hello\"\n                                                          ++ r)) instead of type Phrase\n\n  ** Maybe you gave too many arguments to mkPhrase\n"

secondTree :: Tree (Int, String)
secondTree =
   Node (0,"PizzaEng.gf:")
    [Node (2,"PizzaEng.gf:29:")
      [Node (4,"Happened in linearization of Hello")
        [Node (6,"A function type is expected for mkPhrase (happily (\"hello\"")
          [Node (58,"++ r)) instead of type Phrase") []]]]]
nullTree :: Tree (Int, String)
nullTree = Node (0,"") []
thirdTree :: Tree (Int, String)
thirdTree =
   Node (2,"** Maybe you gave too many arguments to mkPhrase") []

  -- [([Node (0,"PizzaEng.gf:")
  --     [Node (2,"Warning: function Firends is not in abstract") []
  --     ,Node (2,"Warning: category Phr is not in abstract") []
  --     ,Node (2,"Warning: no linearization of Bar") []
  --     ,Node (2,"Warning: no linearization type for Foo, inserting default {s : Str}") []
  --     ,Node (2,"Warning: no linearization type for S, inserting default {s : Str}") []
  --     ]
  --   ,Node (0,"PizzaEng.gf:") [Node (2,"PizzaEng.gf:29:") [Node (4,"Happened in linearization of Hello") [Node (6,"A function type is expected for mkPhrase (happily (\"hello\"") [Node (58,"++ r)) instead of type Phrase") []]]]],Node (0,"** Maybe you gave too many arguments to mkPhrase") []
  --   ]
  --  ,""
  --  )
  -- ]

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


[([],"PizzaEng.gf:\n  Warning: function Firends is not in abstract\n  Warning: category Phr is not in abstract\n  Warning: no linearization of Bar\n  Warning: no linearization type for Foo, inserting default {s : Str}\n  Warning: no linearization type for S, inserting default {s : Str}\nPizzaEng.gf:\n  PizzaEng.gf:29:\n    Happened in linearization of Hello\n      A function type is expected for mkPhrase (happily (\"hello\"\n                                                          ++ r)) instead of type Phrase\n\n  ** Maybe you gave too many arguments to mkPhrase\n")
,([Node (0,"PizzaEng.gf:") [Node (2,"Warning: function Firends is not in abstract") [],Node (2,"Warning: category Phr is not in abstract") [],Node (2,"Warning: no linearization of Bar") [],Node (2,"Warning: no linearization type for Foo, inserting default {s : Str}") [],Node (2,"Warning: no linearization type for S, inserting default {s : Str}") []]],"PizzaEng.gf:\n  PizzaEng.gf:29:\n    Happened in linearization of Hello\n      A function type is expected for mkPhrase (happily (\"hello\"\n                                                          ++ r)) instead of type Phrase\n\n  ** Maybe you gave too many arguments to mkPhrase\n")
,([Node (0,"PizzaEng.gf:") [Node (2,"Warning: function Firends is not in abstract") [],Node (2,"Warning: category Phr is not in abstract") [],Node (2,"Warning: no linearization of Bar") [],Node (2,"Warning: no linearization type for Foo, inserting default {s : Str}") [],Node (2,"Warning: no linearization type for S, inserting default {s : Str}") []],Node (0,"PizzaEng.gf:") [Node (2,"PizzaEng.gf:29:") [Node (4,"Happened in linearization of Hello") [Node (6,"A function type is expected for mkPhrase (happily (\"hello\"") [Node (58,"++ r)) instead of type Phrase") []]]]]],"\n  ** Maybe you gave too many arguments to mkPhrase\n")
,([Node (0,"PizzaEng.gf:") [Node (2,"Warning: function Firends is not in abstract") [],Node (2,"Warning: category Phr is not in abstract") [],Node (2,"Warning: no linearization of Bar") [],Node (2,"Warning: no linearization type for Foo, inserting default {s : Str}") [],Node (2,"Warning: no linearization type for S, inserting default {s : Str}") []],Node (0,"PizzaEng.gf:") [Node (2,"PizzaEng.gf:29:") [Node (4,"Happened in linearization of Hello") [Node (6,"A function type is expected for mkPhrase (happily (\"hello\"") [Node (58,"++ r)) instead of type Phrase") []]]]],Node (0,"** Maybe you gave too many arguments to mkPhrase") []],"")
]


-}
