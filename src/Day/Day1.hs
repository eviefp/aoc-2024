module Day.Day1
  ( run
  ) where

import Control.Applicative (many)
import Data.Biapplicative qualified as Bi
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helpers qualified
import Text.Parser.Token qualified as Token
import Text.Trifecta (Parser)
import Prelude

run :: IO ()
run = do
  Helpers.runDayPart "Day 1, part 1" parse "data/day1-1" (pure . part1)
  Helpers.runDayPart "Day 1, part 2" parse "data/day1-1" (pure . part2)

------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------
data Input = Input
  { left :: [Integer]
  , right :: [Integer]
  }
  deriving stock (Show)

------------------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------------------
parse :: Parser Input
parse = uncurry Input . foldMap singleton <$> many parseLine
 where
  parseLine :: Parser (Integer, Integer)
  parseLine = (,) <$> (Token.decimal <* Token.whiteSpace) <*> (Token.decimal <* Token.whiteSpace)

  singleton :: (Integer, Integer) -> ([Integer], [Integer])
  singleton = Bi.bimap pure pure

------------------------------------------------------------------------------------------
-- Solution
------------------------------------------------------------------------------------------
part1 :: Input -> String
part1 Input {..} =
  show
    . sum
    . fmap abs
    $ zipWith (-) (List.sort left) (List.sort right)

part2 :: Input -> String
part2 Input {..} = show . sum . fmap withLookup $ left
 where
  table :: Map Integer Integer
  table = foldr (\key m -> Map.insertWith (+) key 1 m) Map.empty right

  withLookup :: Integer -> Integer
  withLookup k = (k *) $ maybe 0 id $ Map.lookup k table
