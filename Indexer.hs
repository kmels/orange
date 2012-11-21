module Indexer where

import Database.Redis
import System.Posix.Files(getFileStatus,accessTime,fileID,isDirectory)
import Data.List.Split(splitOn)
import Data.List(inits)

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS
  
-- | Indexes a filepath
indexFilePath :: FilePath -> IO ()
indexFilePath filePath = do
  reditConn <- connect defaultConnectInfo
  fs <- getFileStatus filePath  
  let 
    tokens = concatMap (drop 1 . inits) $ splitOn "/" filePath
    fid = fileID fs
    fat = accessTime fs
    isdir = isDirectory fs
  mapM_ (\token -> runRedis reditConn $ do
            set ("search" <:> show token) (toByteString fid)
            set ("filePath" <:> show fid) (toByteString filePath)
            set ("accessTime" <:> show fid) (toByteString fat)
        ) tokens
        

-- | Intercalates a colon. Used for redis key names.
(<:>) :: String -> String -> ByteString
(<:>) s s2 = BS.pack $ s ++ ":" ++ s2

toByteString :: (Show a) => a -> ByteString
toByteString = BS.pack . show 