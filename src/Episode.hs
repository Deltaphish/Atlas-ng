module Episode ( Episode(Episode)
              , showName
              , seasonName
              , seasonNr
              , episodeName
              , episodeNr
              ) where

import Data.Maybe
import Data.List
import Data.Text (Text)



data Episode = Episode {
    showName :: Maybe Text,
    seasonName :: Maybe Text,
    seasonNr :: Maybe Int,
    episodeName :: Maybe Text,
    episodeNr :: Maybe Int
} deriving (Show,Eq)

sup :: Maybe a -> Maybe a -> Maybe a
Nothing `sup` (Just b) = Just b 
Nothing `sup` Nothing = Nothing
(Just a) `sup` (Just b) = Just b
(Just a) `sup` Nothing = Just a

instance Semigroup Episode where
    (Episode shn1 sn1 snr1 en1 enr1) <> (Episode shn2 sn2 snr2 en2 enr2) =
        Episode (shn1 `sup` shn2) (sn1 `sup` sn2) (snr1 `sup` snr2) (en1 `sup` en2) (enr1 `sup` enr2)
