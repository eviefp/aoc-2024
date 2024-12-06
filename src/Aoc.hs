module Aoc
  ( main
  ) where

import Day.Day1 qualified as Day1
import Day.Day2 qualified as Day2
import Day.Day3 qualified as Day3
import System.Environment qualified as Env
import Prelude

main :: IO ()
main =
  Env.getArgs >>= \case
    ["1"] -> Day1.run
    ["2"] -> Day2.run
    ["3"] -> Day3.run
    _ -> putStrLn "Unknown argument."
