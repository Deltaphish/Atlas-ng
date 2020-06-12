{-# LANGUAGE OverloadedStrings #-}

import Lib
import Sanitize
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators

main :: IO()
main = runAtlas "/home/pi/media/drive/data/torrents/Anime"