{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import PathParserTest
import PathParser

main :: IO()
main = do
    quickCheck prop_parseEpisode

prop_parseEpisode :: EpisodeTest -> Property
prop_parseEpisode (EpisodeTest e _ _ p) = 
    case run p of
        Nothing -> property False
        Just e' -> e === e'



        

