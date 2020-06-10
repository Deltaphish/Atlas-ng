{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Data.Text (Text, dropWhileEnd, cons)
import qualified Data.Text as T
import Data.Char
import Data.Void
import Data.List
import Data.Maybe
import Text.Read
import Sanitize

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

displayEpisode :: Episode -> Text
displayEpisode (Episode shnm snm snr enm enr)
    = T.append "Episode => " $ 
        T.unwords $
            intersperse ":" $ catMaybes [shnm,shnm, T.pack . show <$> snr ,enm, T.pack . show <$> enr]

type Parser = Parsec Void Text

-- Parse "showName - epnr"
parseFileFormat_1 :: Parser Episode
parseFileFormat_1 = do
    shnm <- manyTill printChar (string' "-")
    space
    epnr <- some digitChar
    return $ Episode (Just $ T.pack shnm) Nothing Nothing Nothing (readMaybe epnr)

-- Parse "showName 'ep' epnr"
parseFileFormat_2 :: Parser Episode
parseFileFormat_2 = do
    shnm <- manyTill (satisfy (\c -> isAlphaNum c || c == ' ')) (string' " ep")
    epnr <- skipManyTill space (some digitChar) 
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing Nothing Nothing (readMaybe epnr)

-- Parse "showName - seasonName - epnr"
parseFileFormat_3 :: Parser Episode
parseFileFormat_3 = do
    shnm <- manyTill printChar (string' "-")
    space
    snm <- manyTill printChar (string' "-")
    space
    epnr <- some digitChar
    return $ Episode (Just $ T.strip $ T.pack $ shnm) (Just $ T.strip $ T.pack snm) Nothing Nothing (readMaybe epnr)

    
-- Parse "showName - seasoneName"
parseDirFormat_1 :: Parser Episode
parseDirFormat_1 = do
    shnm <- manyTill printChar (string' " - ")
    space
    snm  <- some printChar
    return $ Episode (Just $ T.strip $ T.pack shnm) (Just $ T.strip $ T.pack snm) Nothing Nothing Nothing

-- Parse "showName '[sS]eason' seasonNr"
parseDirFormat_2 :: Parser Episode
parseDirFormat_2 = do
    shnm <- manyTill (satisfy (\c -> isAlphaNum c || c == ' ')) (string' " Season ")
    space
    snr <- skipManyTill space (some digitChar) 
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing (readMaybe snr) Nothing Nothing



parseEpisodefromFile :: Parser Episode
parseEpisodefromFile = try parseFileFormat_3 <|> try parseFileFormat_1 <|> try parseFileFormat_2

parseEpisodefromDir :: Parser Episode
parseEpisodefromDir = try parseDirFormat_1 <|> try parseDirFormat_2


run :: Text -> Maybe Episode
run t = case parsedFile of
            Nothing -> parsedDir
            Just e -> case parsedDir of
                        Nothing -> Just e
                        Just e'  -> Just $ (e' <> e)
    where
        (d,f) = T.breakOn "/" t
        cleanDir = sanitisePath d
        cleanFile = sanitiseFileName $ T.tail f
        parsedFile = parseMaybe parseEpisodefromFile cleanFile
        parsedDir = parseMaybe parseEpisodefromDir cleanDir
