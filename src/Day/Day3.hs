module Day.Day3
  ( run
  ) where

import Control.Applicative (many)
import Data.Functor (void)
import Data.Monoid (Sum (..))
import Helpers qualified
import Text.Parser.Token qualified as Token
import Text.Trifecta (Parser)
import Text.Trifecta qualified as Trifecta
import Prelude

run :: IO ()
run = do
  Helpers.runDayPart "Day 3, part 1" parse "data/day3-1" (pure . part1)
  Helpers.runDayPart "Day 3, part 2" parse "data/day3-1" (pure . part2)

------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------
data Input = Input
  { input :: String
  }
  deriving stock (Show)

------------------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------------------
parse :: Parser Input
parse = Input <$> many (Trifecta.noneOf [])

------------------------------------------------------------------------------------------
-- Solution
------------------------------------------------------------------------------------------
part1 :: Input -> String
part1 Input {..} =
  show
    . getSum
    . foldMap (Sum . uncurry (*))
    . go
    $ input
 where
  go :: String -> [(Int, Int)]
  go str =
    case str of
      'm' : 'u' : 'l' : '(' : xs ->
        case Trifecta.foldResult (const Nothing) Just $ Trifecta.parseString pairParser mempty xs of
          Just p -> p : go xs
          Nothing -> go xs
      _ : xs -> go xs
      [] -> []

  pairParser :: Parser (Int, Int)
  pairParser = do
    n1 <- Token.integer
    void $ Token.symbol ","
    n2 <- Token.integer
    void $ Token.symbol ")"
    pure (fromIntegral n1, fromIntegral n2)

part2 :: Input -> String
part2 Input {..} =
  show
    . getSum
    . foldMap (Sum . uncurry (*))
    . go True
    $ input
 where
  go :: Bool -> String -> [(Int, Int)]
  go enableMul str =
    case str of
      'm' : 'u' : 'l' : '(' : xs ->
        case Trifecta.foldResult (const Nothing) Just $ Trifecta.parseString pairParser mempty xs of
          Just p ->
            if enableMul
              then p : go enableMul xs
              else go enableMul xs
          Nothing -> go enableMul xs
      'd' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs ->
        go False xs
      'd' : 'o' : '(' : ')' : xs ->
        go True xs
      _ : xs -> go enableMul xs
      [] -> []

  pairParser :: Parser (Int, Int)
  pairParser = do
    n1 <- Token.integer
    void $ Token.symbol ","
    n2 <- Token.integer
    void $ Token.symbol ")"
    pure (fromIntegral n1, fromIntegral n2)
