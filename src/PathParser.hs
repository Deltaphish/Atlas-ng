{-# LANGUAGE OverloadedStrings #-}
module PathParser where

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

import Episode
import Sanitize

type Parser = Parsec Void Text

-- Helper parsers

endingEpisodeNr :: Parser Int
endingEpisodeNr = do
    epnr' <- some digitChar
    space
    eof
    return $ read epnr'

endingEpisodeNrThenTitle :: Parser (Int,T.Text)
endingEpisodeNrThenTitle = do
    spaceChar
    epnr' <- some digitChar
    optional spaceChar
    optional (char '-')
    space
    eptitle' <- some printChar
    eof
    return (read epnr',T.strip $ T.pack eptitle')

parseSE :: Parser (Int,Int)
parseSE = try parseSE_SnEn <|> try parseSE_nxn <|> try paseSE_N_En
    where
        parseSE_SnEn = do
            space
            optional (char '-')
            char' 's'
            seasonNr <- some digitChar
            space
            optional (char '-')
            space
            optional (char' 'e')
            episodeNr <- some digitChar
            return (read seasonNr, read episodeNr)

        parseSE_nxn = do
            space
            optional (char '-')
            seasonNr <- some digitChar
            char' 'x'
            episodeNr <- some digitChar
            return (read seasonNr, read episodeNr)

        paseSE_N_En = do
            optional printChar
            seasonNr <- some digitChar
            some spaceChar :: Parser String
            char' 'e'
            episodeNr <- some digitChar
            some spaceChar
            return (read seasonNr, read episodeNr)

-- Filename parsers

-- Parse "showName - epnr"
parseFileFormat_1 :: Parser Episode
parseFileFormat_1 = do
    shnm <- printChar `someTill` string' "-"
    space
    epnr <- some digitChar
    return $ Episode (Just $ T.pack shnm) Nothing Nothing Nothing (readMaybe epnr)

-- Parse "showName 'ep' epnr"
parseFileFormat_2 :: Parser Episode
parseFileFormat_2 = do
    shnm <- printChar `someTill` string' " ep"
    epnr <- skipManyTill space (some digitChar) 
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing Nothing Nothing (readMaybe epnr)

-- Parse "showName - seasonName - epnr"
parseFileFormat_3 :: Parser Episode
parseFileFormat_3 = do
    shnm <- printChar `someTill` string' "-"
    space
    snm <- printChar `someTill` try (lookAhead (char '-' >> space >> endingEpisodeNr))
    char '-'
    space
    epnr <- some digitChar
    return $ Episode (Just $ T.strip $ T.pack shnm) (Just $ T.strip $ T.pack snm) Nothing Nothing (readMaybe epnr)

-- Parse "showName - SE - eptitle"
parseFileFormat_4 :: Parser Episode
parseFileFormat_4 = do
    shnm <- printChar `someTill` string' "-"
    space
    (snr,epnr) <- parseSE
    space
    optional (char '-')
    epnm <- some printChar
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing (Just snr) (Just $ T.strip $ T.pack epnm) (Just epnr)

-- Parse "Showname 'Episode' NR eptitle"
parseFileFormat_5 :: Parser Episode
parseFileFormat_5 = do
    shnm <- printChar `someTill` string' "Episode"
    space
    epnr <- some digitChar
    space
    epnm <- some printChar
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing Nothing (Just $ T.strip $ T.pack $ epnm) (readMaybe epnr)
    
-- Parse "Showname 'Episode' NR"
parseFileFormat_6 :: Parser Episode
parseFileFormat_6 = do
    shnm <- printChar `someTill` string' "Episode"
    space
    epnr <- some digitChar
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing Nothing Nothing (readMaybe epnr)

-- Parse "Showname NR"
parseFileFormat_7 :: Parser Episode
parseFileFormat_7 = do
    shnm <- printChar `someTill` (try $ lookAhead endingEpisodeNr)
    epnr <- endingEpisodeNr
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing Nothing Nothing (Just epnr)

-- Parse "Showname 'S'SNR NR"
parseFileFormat_8 :: Parser Episode
parseFileFormat_8 = do
    shnm <- printChar `someTill` (try $ lookAhead parseSE)
    (snr,epnr) <- parseSE
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing (Just snr) Nothing (Just epnr)

-- Parse "EpNR"
parseFileFormat_9 :: Parser Episode
parseFileFormat_9 = Episode Nothing Nothing Nothing Nothing . Just <$> endingEpisodeNr

-- Parse "'Episode' EpNR - EPtTitle"
parseFileFormat_10 :: Parser Episode
parseFileFormat_10 = do
    string' "episode"
    space
    epnr <- some digitChar
    space
    optional (char '-')
    epnm <- some printChar
    return $ Episode Nothing Nothing Nothing (Just $ T.strip $ T.pack epnm) (readMaybe epnr)

-- Parse format 'SnrXEpnr title'
parseFileFormat_11 :: Parser Episode
parseFileFormat_11 = do
    (snr,epnr) <- parseSE
    space
    optional (char '-')
    space
    epnm <- some printChar
    return $ Episode Nothing Nothing (Just snr) (Just $ T.strip $ T.pack epnm) (Just epnr)

-- Parse "showName - Ep - eptitle"
parseFileFormat_12 :: Parser Episode
parseFileFormat_12 = do
    shnm <- printChar `someTill` (try $ lookAhead endingEpisodeNrThenTitle)
    (epnr,eptitle) <- endingEpisodeNrThenTitle
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing Nothing (Just eptitle) (Just epnr)

-- Directoryname parsers

-- Parse "showName - seasoneName"
parseDirFormat_1 :: Parser Episode
parseDirFormat_1 = do
    shnm <- printChar `someTill` string' " - "
    space
    snm  <- some printChar
    return $ Episode (Just $ T.strip $ T.pack shnm) (Just $ T.strip $ T.pack snm) Nothing Nothing Nothing

-- Parse "showName '[sS]eason' seasonNr"
parseDirFormat_2 :: Parser Episode
parseDirFormat_2 = do
    shnm <- printChar `someTill` string' " Season "
    space
    snr <- some digitChar
    return $ Episode (Just $ T.strip $ T.pack shnm) Nothing (readMaybe snr) Nothing Nothing

-- Create a super parser
-- Order is important. since some parasers are more vague than others
-- More specific parsers are tried first before resorting to parsers with less yeild
parseEpisodefromFile :: Parser Episode
parseEpisodefromFile = try parseFileFormat_9 <|>
                    try parseFileFormat_10 <|>
                    try parseFileFormat_11 <|>
                    try parseFileFormat_4 <|>
                    try parseFileFormat_5 <|>
                    try parseFileFormat_6 <|>
                    try parseFileFormat_3 <|>
                    try parseFileFormat_1 <|>
                    try parseFileFormat_12 <|>
                    try parseFileFormat_2 <|>
                    try parseFileFormat_8 <|>
                    parseFileFormat_7

parseEpisodefromDir :: Parser Episode
parseEpisodefromDir = try parseDirFormat_1 <|> try parseDirFormat_2


-- run parse on dir and file, then combine the result.
runP :: Text -> Maybe Episode
runP t = case parsedFile of
            Nothing -> parsedDir
            Just e -> case parsedDir of
                        Nothing -> Just e
                        Just e'  -> Just (e' <> e)
    where
        (d,f) = T.breakOn "/" t
        cleanDir = sanitisePath d
        cleanFile = case f of
                    "" -> ""
                    f' -> sanitiseFileName $ T.tail f'
        parsedFile = parseMaybe parseEpisodefromFile cleanFile
        parsedDir = parseMaybe parseEpisodefromDir cleanDir