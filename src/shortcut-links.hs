module Main where


import Text.Pandoc.Filters.ShortcutLinks
import Text.Pandoc.JSON


main = toJSONFilter shortcutLinks
