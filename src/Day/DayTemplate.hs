module Day.Day2
  ( run
  ) where

import Helpers qualified
import Text.Trifecta (Parser)
import Text.Trifecta qualified as Trifecta
import Prelude

run :: IO ()
run = do
  Helpers.runDayPart "Day 2, part 1" parse "data/day2-1" part1
  Helpers.runDayPart "Day 2, part 2" parse "data/day2-1" part2

------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------
data Input

------------------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------------------
parse :: Parser Input
parse = undefined

------------------------------------------------------------------------------------------
-- Solution
------------------------------------------------------------------------------------------
part1 :: Input -> IO String
part1 _ = undefined

part2 :: Input -> IO String
part2 _ = undefined
