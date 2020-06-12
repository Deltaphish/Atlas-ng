{-# LANGUAGE OverloadedStrings #-}

module Lib where

import PathParser
import Episode
import Sanitize

import Data.List
import qualified Data.ByteString as B
import System.Directory
import Control.Monad

import Codec.Binary.UTF8.String

import Database.HDBC
import Database.HDBC.Sqlite3

import qualified Data.Text as T

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource
import Control.Monad.IO.Class


atlasPipeline :: FilePath -> Statement -> Statement -> ConduitT () Void (ResourceT IO) ()
atlasPipeline start s1 s2 = CC.sourceDirectoryDeep False start .|
                                CC.filter is_media              .|
                                CC.map parse                 .|
                                CC.concatMapAccum (\a s -> (s+1, [(s,fst a,snd a)])) 0 .|
                                CC.map genSql .|
                                CC.mapM_ (exe s1 s2)


                        

runAtlas :: FilePath -> IO()
runAtlas start = liftIO $ do
    conn <- connectSqlite3 "media.db"
    initTable conn
    s1 <- prepare conn "INSERT INTO mediafiles VALUES (?,?)"
    s2 <- prepare conn "INSERT INTO episodes VALUES (?,?,?,?,?,?)"
    runConduitRes $ atlasPipeline start s1 s2
    commit conn
    disconnect conn
    return ()
    



genSql :: (Int,FilePath,(Maybe Episode)) -> ([SqlValue],[SqlValue])
genSql xs = (,) [toSql id, toSql fp] ep
    where
        (id,fp,mep) = xs
        ep = case mep of
                Nothing -> []
                Just e -> toSql id : episodeToSql e

exe :: Statement -> Statement -> ([SqlValue],[SqlValue]) -> ResourceT IO () 
exe s1 _ (fp,[]) = (liftIO $ execute s1 fp) >> (return ())
exe s1 s2 (fp,ep) = do
    liftIO $ execute s1 fp
    liftIO $ execute s2 ep
    return ()
        

data MediaFile = MediaFile FilePath (Maybe Episode) deriving Show

is_media :: FilePath -> Bool
is_media f = or $ map ( \a -> isSuffixOf a f) [".mkv",".avi",".webm"]

parse :: FilePath -> (FilePath,(Maybe Episode))
parse f = (f,(runP f'))
    where
        fileNodes = T.splitOn "/" $ T.pack f :: [T.Text]
        f' = T.concat [(last $ init fileNodes),"/",(last fileNodes)]


search :: FilePath -> IO([FilePath])
search f = do
    fs' <- listDirectory f
    let fs = map (\a -> f ++ "/" ++ a) fs'
    files <- filterM doesFileExist fs
    dirs <- filterM doesDirectoryExist fs
    let media_files = filter is_media files
    others <- sequence $ map search dirs
    return $ media_files ++ (concat others)
{-
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

-}

initTable :: Connection -> IO Integer
initTable conn = do
    run conn "CREATE TABLE IF NOT EXISTS mediafiles(id INTEGER PRIMARY KEY AUTOINCREMENT,path TEXT NOT NULL);" []
    run conn "CREATE TABLE IF NOT EXISTS episodes(id INTEGER, showName TEXT, seasonName TEXT, seasonNr INTEGER, episodeName TEXT, episodeNr INTEGER, FOREIGN KEY (id) REFERENCES mediafiles(id));" []
    run conn "SELECT max(id) from mediafiles;" []


--stmt1 = prepare conn "INSERT INTO mediafiles VALUES (?,?)"
--stmt2 = prepare conn "INSERT INTO episodes VALUES (?,?,?,?,?,?)"
    






episodeToSql :: Episode -> [SqlValue]
episodeToSql e = [ toSql (showName e)
                , toSql (seasonName e)
                , toSql (seasonNr e)
                , toSql (episodeName e)
                , toSql (episodeNr e)
                ]