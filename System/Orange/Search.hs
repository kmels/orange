module System.Orange.Search where

import           Control.Applicative   ((<|>))
import           Data.Either           (rights)
import           Data.Maybe            (catMaybes, fromJust)
import           System.Orange.Indexer ((<:>))

import qualified Data.ByteString.Char8 as BS
import           Database.Redis

-- | Searches for filepaths given a query
searchFilePaths :: String -> Connection -> Int -> Int -> IO [BS.ByteString]
searchFilePaths query redisConn from resultsCount = do
  fids <- runRedis redisConn $ do
    zrange ("search" <:> ("*" ++ query ++ "*")) (fromIntegral from) (fromIntegral resultsCount) -- :: Either Reply [BS.ByteString]
  case fids of
    (Right fids') -> do
      filePathMaybes <- mapM (\fid -> runRedis redisConn $ do
                                 get ("filePath" <:> BS.unpack fid)
                             ) fids'
      return $ catMaybes . rights $ filePathMaybes
    _ -> return []

searchPaginated :: String -> Int -> Int -> IO [String]
searchPaginated query s c = do
  conn <- connect defaultConnectInfo
  results <- searchFilePaths query conn s c
  return $ map BS.unpack results

