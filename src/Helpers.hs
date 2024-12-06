module Helpers
  ( runDayPart
  , space
  , newline
  , safeTail
  ) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (..))
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty
import System.IO (stdout)
import Text.Parser.Char qualified as Char
import Text.Parser.Combinators (skipMany)
import Text.Trifecta (Parser)
import Text.Trifecta qualified as Trifecta
import Prelude

runDayPart :: (MonadIO m) => String -> Parser a -> FilePath -> (a -> m String) -> m ()
runDayPart text parser file part = do
  liftIO $ putStr $ text <> ": "
  Trifecta.parseFromFileEx parser file
    >>= Trifecta.foldResult renderParseError (liftIO . putStrLn <=< part)

renderParseError :: (MonadIO m) => Trifecta.ErrInfo -> m ()
renderParseError xs =
  liftIO $ Pretty.renderIO stdout $ renderPretty 0.8 80 $ (Trifecta._errDoc xs) <> Pretty.line' -- TODO: retrieve columns
 where
  renderPretty ribbonFraction page =
    Pretty.layoutSmart
      Pretty.LayoutOptions {layoutPageWidth = Pretty.AvailablePerLine page ribbonFraction}

space :: Parser ()
space = skipMany $ Char.char ' '

newline :: Parser ()
newline = skipMany $ Char.char '\n'

safeTail :: [a] -> [a]
safeTail = \case
  [] -> []
  (_ : xs) -> xs
