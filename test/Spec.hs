{-# LANGUAGE OverloadedStrings #-}

import Lib
import Test.QuickCheck
import Test.QuickCheck.Utf8
import Data.Text.Arbitrary
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Maybe
import Data.List
import Data.Char
import Sanitize

genValidName :: Gen T.Text
genValidName = genValidUtf81 `suchThat` (all isAlphaNum . T.unpack) 

instance Arbitrary Episode where
    arbitrary = do
        shnm <- genValidName
        snm <- genValidName
        epnm <- genValidName
        snr <- choose (0,999)
        epnr <- choose (0,999)
        return $ Episode (Just shnm) (Just snm) (Just snr) (Just epnm) (Just epnr)

type Path = T.Text
data EpisodeTest = EpisodeTest Episode Path deriving Show

instance Arbitrary EpisodeTest where
    arbitrary = do
        e <- arbitrary :: Gen Episode
        dirName <- oneof [genDirFormat_1 e, genDirFormat_2 e]
        fileName <- oneof [genFileFormat_1 e, genFileFormat_2 e]
        return $ EpisodeTest e $ T.concat [dirName,"/",fileName]

compareEpisode :: Episode -> Episode -> Bool
compareEpisode (Episode rshn rsen rsnr repn renr) (Episode shn sen snr epn enr) =
        all compare paired where
        refrence = [rshn, rsen, T.pack.show <$> rsnr, repn, T.pack.show <$> renr]
        result   = [shn, sen, T.pack.show <$> snr, epn, T.pack.show <$> enr]
        paired  = zip refrence result
        compare (_,Nothing) = True
        compare (Just a,Just b) = a == b
        compare _ = error "Empty refrence field in compareEpisode"

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

prop_parseEpisode :: EpisodeTest -> Bool
prop_parseEpisode (EpisodeTest e p) = 
    case run p of
        Nothing -> False
        Just e' -> compareEpisode e e'



        

