{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.Filters.Utils
(
  stringify,
)
where


import Text.Pandoc.Definition
import Text.Pandoc.Walk
 

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: Walkable Inline a => a -> String
stringify = query go . walk deNote
  where go :: Inline -> [Char]
        go Space = " "
        go (Str x) = x
        go (Code _ x) = x
        go (Math _ x) = x
        go LineBreak = " "
        go _ = ""
        deNote (Note _) = Str ""
        deNote x = x
