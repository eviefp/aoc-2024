module Day.Day4
  ( run
  ) where

import Control.Applicative (many, some)
import Data.List.Extra (enumerate)
import Data.Set qualified as Set
import Evie.Grid (Direction, Grid (Grid), Point)
import Evie.Grid qualified as Grid
import Helpers qualified
import Text.Parser.Char qualified as Char
import Text.Trifecta (Parser)
import Prelude hiding (reverse)

run :: IO ()
run = do
  Helpers.runDayPart "Day 4, example part1" parse "data/day4-i" (pure . part1)
  Helpers.runDayPart "Day 4, example part2" parse "data/day4-i" (pure . part2)
  Helpers.runDayPart "Day 4, part 1" parse "data/day4-1" (pure . part1)
  Helpers.runDayPart "Day 4, part 2" parse "data/day4-1" (pure . part2)

------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------
data Input = Input
  { input :: Grid Char
  }
  deriving stock (Show)

------------------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------------------
parse :: Parser Input
parse = Input . Grid <$> many (parseLine <* Helpers.newline)
 where
  parseLine :: Parser [Char]
  parseLine = some (Char.noneOf ['\n'])

------------------------------------------------------------------------------------------
-- Solution
------------------------------------------------------------------------------------------
part1 :: Input -> String
part1 Input {..} =
  show
    . length
    . foldMap (findXMAS "XMAS" input)
    $ enumerate

part2 :: Input -> String
part2 Input {..} =
  show
    . length
    . filter (isXmas input)
    . Set.toList
    . Set.fromList
    . foldMap (findXMAS "MAS" input)
    $ enumerate

findXMAS :: String -> Grid Char -> Direction -> [Point]
findXMAS what grid dir = foldMap (go what) $ Grid.points grid
 where
  go :: [Char] -> Point -> [Point]
  go needle point =
    case (needle, Grid.getValue grid point) of
      ([], _) -> foldMap revPoint $ revPoint point
      (_, Nothing) -> []
      (n : ns, Just h)
        | n == h -> maybe (if null ns then revPoint point else []) (go ns) . Grid.move grid dir $ point
        | otherwise -> []

  revPoint :: Point -> [Point]
  revPoint = maybe [] pure . Grid.move grid (Grid.reverse dir)

isXmas :: Grid Char -> Point -> Bool
isXmas grid pa =
  let
    get dir = Grid.move grid dir pa >>= Grid.getValue grid
  in
    case (,,,) <$> get Grid.NorthWest <*> get Grid.NorthEast <*> get Grid.SouthEast <*> get Grid.SouthWest of
      Nothing -> False
      Just ('M', 'M', 'S', 'S') -> True
      Just ('S', 'M', 'M', 'S') -> True
      Just ('S', 'S', 'M', 'M') -> True
      Just ('M', 'S', 'S', 'M') -> True
      _ -> False
