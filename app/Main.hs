{-# LANGUAGE LambdaCase #-}
import Reactor (run)
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))


main :: IO ()
main = do
  run >>= \case
    0 -> exitSuccess
    c -> exitWith . ExitFailure $ c
