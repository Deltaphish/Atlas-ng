{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}

import Lib
import Test.QuickCheck
import Test.QuickCheck.Utf8
import Data.Text.Arbitrary
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe
import Data.List
import Data.Char
import Data.Bits
import Sanitize

genValidName :: Gen T.Text
genValidName = genValidUtf81 `suchThat` (all isAlphaNum . T.unpack)

data Format = F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | D1 | D2 deriving (Eq,Show)

formatGen :: Format -> (Episode -> Gen T.Text)
formatGen F1 = genFileFormat_1
formatGen F2 = genFileFormat_2
formatGen F3 = genFileFormat_3
formatGen F4 = genFileFormat_4
formatGen F5 = genFileFormat_5
formatGen F6 = genFileFormat_6
formatGen F7 = genFileFormat_7
formatGen F8 = genFileFormat_8
formatGen F9 = genFileFormat_9
formatGen D1 = genDirFormat_1
formatGen D2 = genDirFormat_2

data EpisodeTest = EpisodeTest Episode Format Format T.Text deriving Show

-- A bitfield for each format showing what information should be accesable from parsing
bitmap :: Format -> Int
bitmap F1 = 0b10001
bitmap F2 = 0b10001
bitmap F3 = 0b11001
bitmap F4 = 0b10111
bitmap F5 = 0b10011
bitmap F6 = 0b10001
bitmap F7 = 0b10001
bitmap F8 = 0b10101
bitmap F9 = 0b00001
bitmap D1 = 0b11000
bitmap D2 = 0b10100

boolToMaybe :: Maybe a -> Bool -> Maybe a
boolToMaybe Nothing _ = Nothing
boolToMaybe (Just a) b
    | b        = Just a
    | otherwise = Nothing

clearFields :: Int -> Episode -> Episode
clearFields bits ep =
    Episode
        (boolToMaybe (showName ep)    (testBit bits 4))
        (boolToMaybe (seasonName ep) (testBit bits 3))
        (boolToMaybe (seasonNr ep)   (testBit bits 2))
        (boolToMaybe (episodeName ep) (testBit bits 1))
        (boolToMaybe (episodeNr ep)   (testBit bits 0))
         

formatEpisode :: Episode -> Format -> Format -> Episode
formatEpisode ep f d = clearFields fields ep
    where
        fields = bitmap f .|. bitmap d

instance Arbitrary Episode where
    arbitrary = do
        shnm <- genValidName
        snm <- genValidName
        epnm <- genValidName
        snr <- choose (0,999)
        epnr <- choose (0,999)
        return $ Episode (Just shnm) (Just snm) (Just snr) (Just epnm) (Just epnr)

instance Arbitrary EpisodeTest where
    arbitrary = do
        e <- arbitrary :: Gen Episode
        dirFormat <- elements [D1,D2]
        dir <- formatGen dirFormat e
        fileFormat <- elements [F1,F2,F3,F4,F5,F6,F7,F8,F9]
        file <- formatGen fileFormat e
        let fEpisode = formatEpisode e dirFormat fileFormat
        return $ EpisodeTest fEpisode dirFormat fileFormat $ T.concat [dir,"/",file]

generateTag :: Gen T.Text
generateTag = do
    (start,end) <- elements [ ('(',')'), ('[',']') ]
    tagContent <- genValidName
    return $ T.concat [T.singleton start, tagContent, T.singleton end]

genFileFormat_1 :: Episode -> Gen T.Text
genFileFormat_1 e = do
    let shnm = fromJust $ showName e
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    tag1 <- generateTag
    tag2 <- generateTag
    tag3 <- generateTag
    return $ T.concat [tag1,shnm,tag2,"-",tag3,epnr,".mkv"]

genFileFormat_2 :: Episode -> Gen T.Text
genFileFormat_2 e = do
    let shnm = fromJust $ showName e
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    tag1 <- generateTag
    tag2 <- generateTag
    tag3 <- generateTag
    return $ T.concat [tag1,shnm,tag2," ep ",epnr,".mkv"]

genFileFormat_3 :: Episode -> Gen T.Text
genFileFormat_3 e = do
    let shnm = fromJust $ showName e
    let snm  = fromJust $ seasonName e
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    tag1 <- generateTag
    tag2 <- generateTag
    tag3 <- generateTag
    return $ T.concat [tag1,shnm," - ",snm,tag2," - ",tag3," ",epnr,".mkv"]

genFileFormat_4 :: Episode -> Gen T.Text
genFileFormat_4 e = do
    let shnm = fromJust $ showName e
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    let epnm = fromJust $ episodeName e
    let senr = T.pack $ show $ fromJust $ seasonNr e
    tag1 <- generateTag
    tag2 <- generateTag
    tag3 <- generateTag
    return $ T.concat [tag1,shnm," - S",senr,"E",epnr," ",epnm," ",tag2,tag3,".mkv"]

genFileFormat_5 :: Episode -> Gen T.Text
genFileFormat_5 e = do
    let shnm = fromJust $ showName e
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    let epnm = fromJust $ episodeName e
    tag1 <- generateTag
    tag2 <- generateTag
    return $ T.concat [tag1,shnm," Episode ",epnr," ",epnm,tag2,".mkv"]

genFileFormat_6 :: Episode -> Gen T.Text
genFileFormat_6 e = do
    let shnm = fromJust $ showName e
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    tag1 <- generateTag
    tag2 <- generateTag
    return $ T.concat [tag1,shnm," Episode ",epnr,".mkv"]

genFileFormat_7 :: Episode -> Gen T.Text
genFileFormat_7 e = do
    let shnm = fromJust $ showName e
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    tag1 <- generateTag
    tag2 <- generateTag
    return $ T.concat [tag1,shnm," ",epnr,".mkv"]

genFileFormat_8 :: Episode -> Gen T.Text
genFileFormat_8 e = do
    let shnm = fromJust $ showName e
    let senr = T.pack $ show $ fromJust $ seasonNr e
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    tag1 <- generateTag
    tag2 <- generateTag
    return $ T.concat [tag1,shnm," S",senr, " ", epnr, ".mkv"]

genFileFormat_9 :: Episode -> Gen T.Text
genFileFormat_9 e = do
    let epnr = T.pack $ show $ fromJust $ episodeNr e
    tag1 <- generateTag
    return $ T.concat [tag1,epnr,".mkv"]

genDirFormat_1 :: Episode -> Gen T.Text
genDirFormat_1 e = do
    let shnm = fromJust $ showName e
    let snm  = fromJust $ seasonName e
    tag1 <- generateTag
    tag2 <- generateTag
    return $ T.concat [tag1,shnm," - ",snm,tag2]

genDirFormat_2 :: Episode -> Gen T.Text
genDirFormat_2 e = do
    let shnm = fromJust $ showName e
    let snr  = T.pack $ show $ fromJust $ seasonNr e
    tag1 <- generateTag
    tag2 <- generateTag
    return $ T.concat [tag1,shnm," Season ",snr,tag2] 


main :: IO()
main = do
    quickCheck prop_parseEpisode

prop_parseEpisode :: EpisodeTest -> Property
prop_parseEpisode (EpisodeTest e _ _ p) = 
    case run p of
        Nothing -> property False
        Just e' -> e === e'



        

