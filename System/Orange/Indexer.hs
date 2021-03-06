module System.Orange.Indexer where

import           Data.List             (inits)
import           Data.List.Split       (splitOn)
import           Database.Redis
import           System.Posix.Files    (accessTime, fileID, getFileStatus,
                                        isDirectory, modificationTime)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Foreign.C.Types       (CTime (..))
import           System.Posix.Types    (EpochTime)
import           Unsafe.Coerce

import           Control.Exception     (IOException (..), try)

-- | Indexes something about the file in a redis database
indexFilePath :: FilePath -> Connection -> IO ()
indexFilePath filePath redisConn = do
  putStr $ "indexing " ++ filePath
  fsEither <- try $ getFileStatus filePath  --get file status
  case fsEither of
    Left e -> do
      putStrLn $ "... error, not indexed: " ++ show (e :: IOException)
    Right fs -> do
      let
        tokens = concatMap (drop 1 . inits) $ splitOn "/" filePath
        fid = fileID fs
        fat = accessTime fs
        fmt = modificationTime fs

      _ <- runRedis redisConn $ do
        set ("accessTime" <:> show fid) (toByteString fat)
        set ("modificationTime" <:> show fid) (toByteString fmt)
        --useful for searching with wildcards
        --e.g. for queries like dir/dir'
        set ("filePath" <:> filePath) (BS.pack . show $ fid)

      mapM_ (\token -> runRedis redisConn $ do
                -- sort by modification date
                zadd ("search" <:> token) ([(realToFrac fmt, toByteString fid)])
            --we've just accessed it so all have the same access time
            ) tokens
      putStrLn " ... done "

-- | Intercalates a colon. Used for redis key names.
(<:>) :: String -> String -> ByteString
(<:>) s s2 = BS.pack $ s ++ ":" ++ s2

toByteString :: (Show a) => a -> ByteString
toByteString = BS.pack . show
