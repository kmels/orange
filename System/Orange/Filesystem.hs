----------------------------------------------------------------------------
-- |
-- Module      :  System.Orange.Filesystem
-- Copyright   :  (c) Carlos López-Camey
-- License     :  BSD-3
--
-- Maintainer  :  c.lopez@kmels.net
-- Stability   :  stable
--
--
-- Functions that interact with the filesystem
-----------------------------------------------------------------------------
{-# LANGUAGE DoAndIfThenElse #-}

module System.Orange.Filesystem where

import           Control.Monad         (filterM, when)
import           Data.List             (isPrefixOf, partition)

import           System.Directory      (getHomeDirectory,getDirectoryContents,doesDirectoryExist)

import           Control.Exception     (IOException (..), try)
import           System.IO.Unsafe(unsafePerformIO)

import           Data.ByteString.Char8 (unpack)

import           Text.Regex.TDFA
import           System.INotify

-- | Gives a list of subdirectories given a directory
subfiles :: FilePath -> IO ([FilePath],[FilePath])
subfiles fp = do
  isDir <- doesDirectoryExist fp
  isIgnored <- isIgnored fp
  if (isDir && not isIgnored)
  then do
       children' <- try $ getDirectoryContents fp :: IO (Either IOException [FilePath])

       let
          children = case children' of
             Left e -> []
             Right cs -> filter (`notElem` [".",".."]) cs
          childrenFilePaths = map (fp </>) children

       let (directoryFilePaths, normalFilePaths) = partition (unsafePerformIO . doesDirectoryExist) childrenFilePaths

       allDescendants <- mapM subfiles directoryFilePaths

       return $ let
                  dirs = concatMap fst allDescendants
                  files = concatMap snd allDescendants
                in (directoryFilePaths ++ dirs, normalFilePaths ++ files)
  else return ([],[])
  
watch :: FilePath -> INotify -> IO WatchDescriptor
watch dirFilePath inotify = addWatch
                 inotify
                 [AllEvents]
                 dirFilePath
                 print
                 
-- | Checks for expressions in ~/.orange
isIgnored :: FilePath -> IO Bool
isIgnored fp = do
  homeDirectory <- getHomeDirectory
  ignoreRegex <- readFile $ homeDirectory </> ".orange"
  let matches = (any (fp =~) $ lines ignoreRegex)
  when matches $ putStrLn $ fp ++ " was ignored"
  --when (not matches) $ putStrLn " was not ignored"
  return $ matches
  
(</>) :: FilePath -> FilePath -> FilePath
(</>) parent cfp = parent ++ "/" ++ cfp