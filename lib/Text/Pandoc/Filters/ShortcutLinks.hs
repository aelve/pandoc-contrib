{-# LANGUAGE OverloadedStrings #-}


{- |
Module      : Text.Pandoc.Filters.ShortcutLinks
License     : BSD3
Maintainer  : Artyom <yom@artyom.me>

This filter turns links that look like “[something](\@w)” into <https://en.wikipedia.org/wiki/Something>. For details, see <https://github.com/aelve/shortcut-links>.
-}
module Text.Pandoc.Filters.ShortcutLinks
(
  shortcutLinks,
)
where


-- General
import Data.Maybe
import Control.Applicative
-- Parsing
import Text.Parsec hiding (optional, (<|>))
-- Text
import Text.Printf
import qualified Data.Text as T
import Data.Text (Text)
-- Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Walk
-- shortcut-links
import ShortcutLinks
-- local
import Text.Pandoc.Filters.Utils


shortcutLinks :: Inline -> IO Inline
shortcutLinks i@(Link attr is (url, title)) | '@':_ <- url = do
  -- %20s are introduced by Pandoc and needs to be converted back to spaces
  let urlOriginalT = T.replace "%20" " " (T.pack url)
      urlOriginalS = T.unpack urlOriginalT
  case parseLink urlOriginalT of
    Left err -> do
      printf "'%s' is not a proper shortcut link: %s\n" urlOriginalS err
      return i
    Right (shortcut, option, text) -> do
      let shortcut' = shortcut
          option'   = option
          text'     = fromMaybe (T.pack (stringify is)) text
      case useShortcut shortcut' option' text' of
        Success link -> return (Link attr is (T.unpack link, title))
        Warning warnings link -> do
          printf "Warnings when processing a shortcut link (%s):\n"
                 urlOriginalS
          mapM_ putStrLn warnings
          return (Link attr is (T.unpack link, title))
        Failure err -> do
          error $ printf "Error when processing a shortcut link (%s): %s\n"
                         urlOriginalS err
shortcutLinks other = return other

-- | Parse a shortcut link. Allowed formats:
--
-- @
-- \@name
-- \@name:text
-- \@name(option)
-- \@name(option):text
-- @
parseLink :: Text -> Either String (Text, Maybe Text, Maybe Text)
parseLink = either (Left . show) Right . parse p ""
  where
    shortcut = some (alphaNum <|> char '-')
    option   = char '(' *> some (noneOf ")") <* char ')'
    text     = char ':' *> some anyChar
    p = do
      char '@'
      (,,) <$> T.pack <$> shortcut
           <*> optional (T.pack <$> option)
           <*> optional (T.pack <$> text)
