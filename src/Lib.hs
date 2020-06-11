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
    shnm <- printChar `someTill` (string' "-")
    space
    snm <- printChar `someTill` (try $ lookAhead $ (char '-' >> space >> endingEpisodeNr))
    char '-'
    space
    epnr <- some digitChar
    return $ Episode (Just $ T.strip $ T.pack $ shnm) (Just $ T.strip $ T.pack snm) Nothing Nothing (readMaybe epnr)

parseSE :: Parser (Int,Int)
parseSE = try parseSE_SnEn <|> try parseSE_nxn
    where
        parseSE_SnEn = do
            char' 's'
            seasonNr <- some digitChar
            space
            optional (char '-')
            space
            optional (char' 'e')
            episodeNr <- some digitChar
            return $ (read seasonNr, read episodeNr)

        parseSE_nxn = do
            seasonNr <- some digitChar
            char' 'x'
            episodeNr <- some digitChar
            return $ (read seasonNr, read episodeNr)

-- Parse "showName - SE - eptitle"
parseFileFormat_4 :: Parser Episode
parseFileFormat_4 = do
    shnm <- manyTill printChar (string' "-")
    space
    (snr,epnr) <- parseSE
    space
    epnm <- some printChar
    return $ Episode (Just $ T.strip $ T.pack $ shnm) Nothing (Just snr) (Just $ T.strip $ T.pack $ epnm) (Just epnr)

-- Parse "Showname 'Episode' NR eptitle"
parseFileFormat_5 :: Parser Episode
parseFileFormat_5 = do
    shnm <- manyTill printChar (string' "Episode")
    space
    epnr <- some digitChar
    space
    epnm <- some printChar
    return $ Episode (Just $ T.strip $ T.pack $ shnm) Nothing Nothing (Just $ T.strip $ T.pack $ epnm) (readMaybe epnr)
    
-- Parse "Showname 'Episode' NR"
parseFileFormat_6 :: Parser Episode
parseFileFormat_6 = do
    shnm <- manyTill printChar (string' "Episode")
    space
    epnr <- some digitChar
    return $ Episode (Just $ T.strip $ T.pack $ shnm) Nothing Nothing Nothing (readMaybe epnr)

-- Parse "Showname NR"
parseFileFormat_7 :: Parser Episode
parseFileFormat_7 = do
    shnm <- printChar `someTill` (try $ lookAhead endingEpisodeNr)
    epnr <- endingEpisodeNr
    return $ Episode (Just $ T.strip $ T.pack $ shnm) Nothing Nothing Nothing (Just epnr)

-- Parse "Showname 'S'SNR NR"
parseFileFormat_8 :: Parser Episode
parseFileFormat_8 = do
    shnm <- printChar `someTill` (try $ lookAhead parseSE)
    (snr,epnr) <- parseSE
    return $ Episode (Just $ T.strip $ T.pack $ shnm) Nothing (Just snr) Nothing (Just epnr)

-- Parse "EpNR"
parseFileFormat_9 :: Parser Episode
parseFileFormat_9 = do
    epnr <- endingEpisodeNr
    return $ Episode Nothing Nothing Nothing Nothing (Just epnr)

-- Parse "'Episode' EpNR - EPtTitle"
parseFileFormat_10 :: Parser Episode
parseFileFormat_10 = do
    string' "episode"
    space
    epnr <- some digitChar
    space
    optional (char '-')
    epnm <- some printChar
    return $ Episode Nothing Nothing Nothing (Just $ T.strip $ T.pack $ epnm) (readMaybe epnr)

-- Parse format 'SnrXEpnr title'
parseFileFormat_11 :: Parser Episode
parseFileFormat_11 = do
    (snr,epnr) <- parseSE
    space
    optional (char '-')
    space
    epnm <- some printChar
    return $ Episode Nothing Nothing (Just snr) (Just $ T.strip $ T.pack $ epnm) (Just epnr)

endingEpisodeNr :: Parser Int
endingEpisodeNr = do
    epnr' <- some digitChar
    space
    eof
    return $ read epnr'

-- Parse "showName - seasoneName"
parseDirFormat_1 :: Parser Episode
parseDirFormat_1 = do
    shnm <- someTill printChar (string' " - ")
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
parseEpisodefromFile = try parseFileFormat_9 <|>
                    try parseFileFormat_10 <|>
                    try parseFileFormat_11 <|>
                    try parseFileFormat_5 <|>
                    try parseFileFormat_6 <|>
                    try parseFileFormat_3 <|>
                    try parseFileFormat_1 <|>
                    try parseFileFormat_4 <|>
                    try parseFileFormat_2 <|>
                    try parseFileFormat_8 <|>
                    parseFileFormat_7

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
