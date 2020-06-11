{-# LANGUAGE OverloadedStrings #-}

module Lib where

import PathParser
import Episode
import Sanitize

import Data.List
import System.Directory
import Control.Monad

import qualified Data.Text as T

data MediaFile = MediaFile FilePath (Maybe Episode) deriving Show

is_media :: FilePath -> Bool
is_media f = or $ map ( \a -> isSuffixOf a f) [".mkv",".avi",".webm"]

search :: FilePath -> IO([FilePath])
search f = do
    fs' <- listDirectory f
    let fs = map (\a -> f ++ "/" ++ a) fs'
    files <- filterM doesFileExist fs
    dirs <- filterM doesDirectoryExist fs
    let media_files = filter is_media files
    others <- sequence $ map search dirs
    return $ media_files ++ (concat others)

parse :: FilePath -> MediaFile
parse f = MediaFile f (runP f')
    where
        fileNodes = T.splitOn "/" $ T.pack f :: [T.Text]
        f' = T.concat [(last $ init fileNodes),"/",(last fileNodes)]

testy :: T.Text -> Bool
testy t = case t of
            "" -> False
            a -> True

run :: FilePath -> IO()
run f = do
    f' <- makeAbsolute f
    fs <- search f'
    mapM (putStrLn.show.parse) fs
    return ()