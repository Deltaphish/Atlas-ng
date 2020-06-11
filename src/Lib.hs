{-# LANGUAGE OverloadedStrings #-}

module Lib where

import PathParser
import Episode
import Sanitize

import Data.List
import System.Directory
import Control.Monad

import Database.HDBC
import Database.HDBC.Sqlite3

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

parse :: FilePath -> (FilePath,(Maybe Episode))
parse f = (f,(runP f'))
    where
        fileNodes = T.splitOn "/" $ T.pack f :: [T.Text]
        f' = T.concat [(last $ init fileNodes),"/",(last fileNodes)]

testy :: T.Text -> Bool
testy t = case t of
            "" -> False
            a -> True

runAtlas :: FilePath -> IO()
runAtlas f = do
    f' <- makeAbsolute f
    fs <- search f'
    conn <- connectSqlite3 "media.db"
    id <- initTable conn 
    insertIntoDB conn (map parse fs) (id+1)
    commit conn
    disconnect conn
    return ()

initTable :: Connection -> IO Integer
initTable conn = do
    run conn "CREATE TABLE IF NOT EXISTS mediafiles(id INTEGER PRIMARY KEY AUTOINCREMENT,path TEXT NOT NULL);" []
    run conn "CREATE TABLE IF NOT EXISTS episodes(id INTEGER, showName TEXT, seasonName TEXT, seasonNr INTEGER, episodeName TEXT, episodeNr INTEGER, FOREIGN KEY (id) REFERENCES mediafiles(id));" []
    run conn "SELECT max(id) from mediafiles;" []

insertIntoDB :: Connection -> [(FilePath,Maybe Episode)] -> Integer -> IO ()
insertIntoDB _ [] _ = return ()
insertIntoDB conn x id = do
    stmt1 <- prepare conn "INSERT INTO mediafiles VALUES (?,?)"
    stmt2 <- prepare conn "INSERT INTO episodes VALUES (?,?,?,?,?,?)"
    foo stmt1 stmt2 x id
    where
        foo :: Statement -> Statement -> [(FilePath, Maybe Episode)] -> Integer -> IO()
        foo _ _ [] _ = return ()
        foo s1 s2 (x:xs) id = do
            execute s1 [toSql id, (toSql.fst) x]
            case snd x of
                Nothing -> return (0)
                Just e -> execute s2 $ (toSql id) : (episodeToSql e)
            foo s1 s2 xs (id+1)






episodeToSql :: Episode -> [SqlValue]
episodeToSql e = [ toSql (showName e)
                , toSql (seasonName e)
                , toSql (seasonNr e)
                , toSql (episodeName e)
                , toSql (episodeNr e)
                ]