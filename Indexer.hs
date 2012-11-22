module Indexer where

import Database.Redis
import System.Posix.Files(getFileStatus,accessTime,fileID,isDirectory,modificationTime)
import Data.List.Split(splitOn)
import Data.List(inits)

import Data.ByteString(ByteString)
import qualified Data.ByteString.Char8 as BS  
  
import System.Posix.Types(EpochTime)
import Foreign.C.Types(CTime(..))
import Unsafe.Coerce
        
-- | Indexes something about the file in a redis database
indexFilePath :: FilePath -> Connection -> IO ()
indexFilePath filePath redisConn = do
  fs <- getFileStatus filePath --get file status  
  let
    tokens = concatMap (drop 1 . inits) $ splitOn "/" filePath
    fid = fileID fs
    fat = accessTime fs
    fmt = modificationTime fs
    
  putStr $ "indexing " ++ filePath
  _ <- runRedis redisConn $ do
    set ("accessTime" <:> show fid) (toByteString fat)
    set ("modificationTime" <:> show fid) (toByteString fmt)
    set ("filePath" <:> show fid) (toByteString filePath)
    
  putStr $ "mod time: " ++ (show fmt)
  mapM_ (\token -> runRedis redisConn $ do
            -- sort by modification date, we've just accessed it so all have the same access time
            zadd ("search" <:> token) ([(unsafeCoerce fmt, toByteString fid)])
        ) tokens
  putStrLn " ... done "

-- | Intercalates a colon. Used for redis key names.
(<:>) :: String -> String -> ByteString
(<:>) s s2 = BS.pack $ s ++ ":" ++ s2

toByteString :: (Show a) => a -> ByteString
toByteString = BS.pack . show 