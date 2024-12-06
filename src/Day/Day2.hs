module Day.Day2
  ( run
  ) where

import Control.Applicative (Alternative (..))
import Data.Bool (bool)
import Data.Monoid (Sum (Sum))
import Helpers qualified
import Text.Parser.Token qualified as Token
import Text.Trifecta (Parser)
import Text.Trifecta qualified as Trifecta
import Prelude

run :: IO ()
run = do
  Helpers.runDayPart "Day 2, part 1" parse "data/day2-1" (pure . part1)
  Helpers.runDayPart "Day 2, part 2" parse "data/day2-1" (pure . part2)

------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------
data Input = Input
  { reports :: [[Integer]]
  }

------------------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------------------
parse :: Parser Input
parse = Input <$> many (Trifecta.try $ parseReport <* Token.whiteSpace)
 where
  parseReport :: Parser [Integer]
  parseReport = (:) <$> Token.decimal <*> many (Helpers.space *> Token.decimal)

------------------------------------------------------------------------------------------
-- Solution
------------------------------------------------------------------------------------------
part1 :: Input -> String
part1 = solve 0

part2 :: Input -> String
part2 = solve 1

solve :: Int -> Input -> String
solve errorMargin Input {..} = show $ foldMap satisfiesRules reports
 where
  satisfiesRules :: [Integer] -> Sum Integer
  satisfiesRules report =
    let
      size = length report
      increasing =
        any (\rep -> size - length rep <= errorMargin)
          . stabilize (takeOnly (\a b -> a < b && isSafe a b))
          $ report
      decreasing =
        any (\rep -> size - length rep <= errorMargin)
          . stabilize (takeOnly (\a b -> a > b && isSafe a b))
          $ report
    in
      bool (Sum 0) (Sum 1) $ increasing || decreasing

  stabilize :: (a -> [a]) -> a -> [a]
  stabilize f a =
    let
      one = f a
      two = concatMap f one
    in
      if length one == length two
        then one
        else concatMap f one

  takeOnly :: (Integer -> Integer -> Bool) -> [Integer] -> [[Integer]]
  takeOnly op =
    \case
      (x1 : x2 : xs) ->
        if op x1 x2
          then (x1 :) <$> takeOnly op (x2 : xs)
          else takeOnly op (x1 : xs) ++ takeOnly op (x2 : xs)
      xs -> [xs]

  isSafe :: Integer -> Integer -> Bool
  isSafe a b =
    let
      diff = abs (a - b)
    in
      diff >= 1 && diff <= 3
