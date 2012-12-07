module Main where

import           System.IO

import           Database.Redis

import           System.Orange.Indexer (indexFilePath)
import           System.Orange.Search  (searchFilePaths)
import           System.Orange.Filesystem  (subfiles)

import           System.Environment

import           System.INotify
import           Data.ByteString.Char8 (unpack)

{-  | Usage

orange index => indexes files + watches
orange watch => watches
orange search abc => output results

-}
main :: IO ()
main = do
    args <- getArgs
    case args of
      ["index"] -> do
        inotify <- initINotify
        print inotify
        home <- return "/mnt/kmels-large-hd/music" --getHomeDirectory

        --get list of home subdirectories
        (directories, files) <- subfiles home
        --indexes subdirectories filepaths
        reditConn <- connect defaultConnectInfo

        mapM_ (flip indexFilePath reditConn) (directories ++ files)
        putStrLn $ "I just indexed " ++ show (length (directories ++ files)) ++ " file paths"
        putStrLn "I'm now watching all the changes occuring to them. Hit enter to terminate"
        getLine

        print ""
      ["search",query] -> do
        conn <- connect defaultConnectInfo
        results <- searchFilePaths query conn 0 (-1)
        mapM_ (putStrLn . unpack) results
      _ -> do
        putStrLn "Error, usage: `orange index` or `orange search <query`"

