{-# LANGUAGE OverloadedStrings #-}
module Sanitize where

import Data.Text (Text, dropWhileEnd, cons)
import qualified Data.Text as T

sanitisePath :: Text -> Text
sanitisePath = T.strip . spacersToSpace . removeTags

sanitiseFileName :: Text -> Text
sanitiseFileName = sanitisePath . removeSuffix

removeSuffix :: Text -> Text
removeSuffix t = T.init $ dropWhileEnd (/= '.') t

spacersToSpace :: Text -> Text
spacersToSpace = T.replace "_" " " . T.replace "." " "

removeTags :: Text -> Text
removeTags t
    | t == T.empty = T.empty
    | c == '['  = removeTags $ removeTag ']' cs
    | c == '('  = removeTags $ removeTag ')' cs
    | otherwise = c `cons` removeTags cs
    where
        (c,cs) = (T.head t,T.tail t)
        removeTag end = T.tail . T.dropWhile (/= end)